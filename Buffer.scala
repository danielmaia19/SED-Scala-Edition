// P1718603X
import ScalaTest.buffer

class Buffer(s: String) {
  
  import scala.collection.mutable.StringBuilder
  
  private var buffer: StringBuilder = new StringBuilder(s)
  private var cursor: Int = 0  // cursor is in between characters
  private var marker: Int = 0  // marker is in between characters
  private var paste: String = ""

  /*
   * A buffer holds a mutable string and maintains a current cursor position
   * (cursor) and a position marker (marker). The cursor (and marker) can occupy
   * any index between zero and the length of the buffer.  We consider the cursor
   * to lie 'between' characters as indicated by the ^ in the diagram below.
   * Initially, the cursor is at position zero.
   * 
   *     B U F F E R
   *    ^              In this example the cursor is at position zero.
   *    0 1 2 3 4 5 6
   *    
   *  The marker is also used to define a region.  For example, mark the current
   *  cursor position; then move the cursor to a new position. E.g.
   *  
   *     B U F F E R         marker = 0
   *    ^_______^            cursor = 4
   *    m       c            This highlights the region between 0 and 4
   *                         i.e. the letters:  BUFF
   */
  
  private def end: Int = buffer.length              // the end of the line
  private def lwr: Int = Math.min(marker, cursor)
  private def upr: Int = Math.max(marker, cursor)
  
  /*
   * Accessor methods to return aspects of the state
  */
  def getCursor: Int = cursor
  def getMarker: Int = marker
  def getString: String = buffer.toString
  def getPaste: String = paste
  
  /**
   * Repeatedly run a sequence of commands
   */
  def rpt (n: Int) ( commands: => Unit ) {
    for (i <- 1 to n) { commands }
  }
  
  /**
   * AL: Arrow Left.  Move the cursor one place to the left.  Zero is the left-most
   * position the cursor can occupy. This operation does not change any other
   * state variables.
   */
  def al() {
    if ( cursor > 0 ) cursor -= 1
  }
  
  /**
   * AR: Arrow Right.  Move the cursor one place to the right.  The cursor cannot
   * move beyond the end of the buffer. This operation does not change any other
   * state variables.
   */
  def ar() {
    if(cursor >= 0 && end > cursor) cursor += 1
  }
  
  /**
   * Go to (move) the cursor to a specific position.  This is limited so that the
   * cursor cannot be below zero or above end. This operation does not change any
   * other state variables.
   */
  def go(n: Int) {
    if ( n >= 0 && n <= end ) cursor = n
    else if ( n < 0 ) cursor = 0
    else cursor = end
  }
  
  /**
   * To Left.  Move the cursor to the left-most (zero) position. This operation
   * does not change any other state variables.
   */
  def tl() {
    cursor = 0
  }
  
  /**
   * To Right. Move the cursor to the right-most (end of buffer) position. This
   * operation does not change any other state variables.
   */
  def tr() {
    cursor = end
  }
  
  /**
   * Define Region.  Save the current cursor position (as marker). Only changes
   * the marker. This operation does not change any other state variables.
   */
  def dr() {
    marker = cursor
  }
  
  /**
   * Define All.  Set the marker to zero, and the cursor to end.  This marks
   * the entire buffer. This operation does not change the paste buffer state.
   */
  def da() {
    marker = 0
    cursor = end
  }
  
  /**
   * Enter String. Insert the given string at the current cursor position.
   * After insertion the cursor moves to the end of the inserted text.  E.g.
   * 
   *     B U F F E R       
   *          ^            cursor = 3
   *
   * Then perform  es("XYZ")
   * 
   *     B U F X Y Z F E R       
   *                ^      cursor = 6
   *
   * This operation modifies the string buffer and the cursor position. The
   * paste buffer and the marker position remain unchanged.
   */
  def es(s: String) {
    buffer.insert(cursor, s)
    cursor += s.length
  }
  
  /**
   * Copy the contents of the marked region to the paste buffer. This operation
   * updates the paste buffer but does not change any other state variables.
   */
  def xc() {

    if (marker > cursor) {

      for(pointer <- cursor to marker - 1) {
        paste += buffer.charAt(pointer).toString
      }

    } else if (cursor > marker) {

      for(pointer <- marker to cursor - 1) {
        paste += buffer.charAt(pointer).toString
      }

    }
  }
  
  /**
   * Delete the contents of the marked region and save the cut string in the paste
   * buffer. This operation re-sets the cursor and the marker to the start of the
   * cut text. For example:
   * 
   *     B U F F E R      marker = 1       
   *      ^     ^         cursor = 4
   *
   * Then perform  xd()
   * 
   *     B E R           marker = 1
   *      ^              cursor = 1
   *
   */
  def xd() {

    if (marker > cursor) {

      for(pointer <- cursor to marker - 1) {
        paste += buffer.charAt(pointer).toString
      }

      buffer.delete(cursor, marker)
      cursor = getCursor
      marker = getCursor

    } else if (cursor > marker) {

      for(pointer <- marker to cursor - 1) {
        paste += buffer.charAt(pointer).toString
      }

      buffer.delete(marker, cursor)
      cursor = getMarker
      marker = getMarker
    }
  }
  
  /**
   * Insert the contents of the paste buffer at the current cursor position.
   * Effectively, this is the same as calling es() with the contents of the
   * paste buffer as the string to be inserted.
   */
  def xp() {
    buffer.insert(getCursor, paste.toString)
    cursor += paste.length
  }
  
  /**
   * Edit Erase  (forward delete).  Delete the character to the right of the
   * cursor. If the cursor is at the end of the buffer then ignore this command
   * (there is no character to the right of the cursor). If deletion succeeds
   * then do not change the cursor position.  The marker position is not
   * changed UNLESS the deletion has caused it to point beyond the end of the
   * (updated) buffer. In this case make the marker equal to end.
   */
  def ee() {
    buffer.delete(cursor, cursor+1)
    marker = end
  }

  /**
   * Edit Delete (backspace/backward delete). Delete the character to the left of
   * the cursor. If the cursor is at the start of the buffer (index zero) then
   * ignore this command (there is no character to the left of the cursor). If
   * deletion succeeds then the cursor is moved one place to the left. The marker
   * position is not changed UNLESS the deletion has caused it to point beyond the
   * end of the (updated) buffer. In this case make the marker equal to end.
   */
  def ed() {

    if (cursor > 0 ) {
      buffer.deleteCharAt(cursor-1)
      cursor -= 1
    }

    if (marker > end ) {
      marker = end
    }

  }

  /**
   * Update the buffer by inverting the case of any alphabetic characters within
   * the defined region.  Only the characters between cursor and marker are
   * therefore affected.  This operation does not change any other state variables.
   */
  def cc() {
    for (pointer <- marker to cursor-1) {
      if(buffer.charAt(pointer).isUpper) {
        buffer.charAt(pointer).toLower
      } else {
        buffer.update(pointer, buffer.charAt(pointer).toUpper)
      }

    }
  }
  
  /**
   * Substitute Characters.  Within the defined region substitute any instances of
   * character s with character t. This operation does not change any other state
   * variables.  For example:
   * 
   *     a a r d v a r k s      marker = 1       
   *      ^           ^         cursor = 7
   *
   * Then perform  sc('a', 'X')
   * 
   *     a X r d v X r k s      marker = 1       
   *      ^           ^         cursor = 7
   */
  def sc(s: Char, t: Char) {
    var temp: String = buffer.toString()
    for (pointer <- marker to cursor -1 ) {
      if (temp.charAt(pointer) == s) {
        buffer.update(pointer, t.toChar)
      }
    }
  }

  /**
   * Delete Duplicate characters.  Within the defined region, for each character,
   * if it occurs once then keep it, but if it occurs multiple times then keep
   * only the first occurrence.  The characters to the left and right of the
   * defined region remain unchanged, but within the defined region the duplicates
   * are removed. This operation does not affect the paste buffer. The cursor is
   * placed finally at the lower end of the defined region and the marker is placed
   * finally at the upper end of the (probably reduced) defined region. For example:
   * 
   *     m i s s i s s i p p i       marker =  1       
   *      ^                 ^        cursor = 10
   *
   * Then perform  sc('a', 'X')
   * 
   *     m i s p i      marker = 1       
   *      ^     ^       cursor = 4
   */
  def dd() {
    buffer.replace(lwr, upr, getString.slice(lwr, upr).distinct)
  }
  
  /**
   * Fast Forwards.  Starting at the current cursor position, locate the next
   * occurrence of the character c. If the cursor reaches the end of the string
   * without finding an occurrence of c then return false. If the cursor finds
   * an occurrence of c then leave the cursor at that position and return true.
   * This operation does not change any variable other than the cursor position.
   * Example:
   *     m i s s i s s i p p i       
   *    ^                          cursor =  0
   *
   * Then perform  ff('i')
   * 
   *     m i s s i s s i p p i            
   *      ^                        cursor =  1: returns true
   *  
   * Then perform  ff('i')
   * 
   *     m i s s i s s i p p i            
   *      ^                        cursor =  1: returns true
   *  
   * Then perform  ff('p')
   * 
   *     m i s s i s s i p p i            
   *                    ^          cursor =  8: returns true
   *                    
   *  Then perform  ff('s')
   * 
   *     m i s s i s s i p p i            
   *                          ^    cursor = 11: returns false
   */
  def ff(c: Char): Boolean = {
    buffer.toString().toUpperCase
    c.toUpper

      for (pointer <- cursor to end-1) {
        if (buffer.charAt(pointer) == c) {
          cursor = pointer
          return true
        } else {
          cursor = end
        }
      }
    return false
  }
  
  /**
   * Find Forwards.  This generalises the character version of find forwards
   * The user provides a predicate to specify the search condition.
   * This operation does not change any variable other than the cursor position.
   */
  def ff(p: Char => Boolean): Boolean = {
    buffer.toString().toLowerCase

    for (pointer <- cursor to end-1) {
      if (p(buffer.charAt(pointer))) {
        cursor = pointer
        return true
      } else {
        cursor = end
      }
    }
    return false
  }
  
  /**
   * Find Backwards.  Starting at the current cursor position, locate the previous
   * occurrence of the character c. If the cursor reaches the start of the string
   * without finding an occurrence of c then return false. If the cursor finds
   * an occurrence of c then leave the cursor at that position and return true.
   * This operation does not change any variable other than the cursor position.
   */
  def fb(c: Char): Boolean = {
    buffer.toString().toUpperCase
    c.toUpper

    for (pointer <- end-1 to cursor) {
      if (buffer.charAt(pointer) == c) {
        println(pointer)
        cursor = pointer
        return true
      } else {
        cursor = end
      }
    }
    return false
  }

  /**
   * Find Backwards.  This generalises the character version of find backwards.
   * The user provides a predicate to specify the search condition.
   * This operation does not change any variable other than the cursor position.
   */
   //TODO: Still need to fix
  def fb(p: Char => Boolean): Boolean = {
    buffer.toString().toUpperCase

    for (pointer <- end to cursor-1) {
      if (p(buffer.charAt(pointer))) {
        cursor = pointer
        return true
      } else {
        cursor = end
      }
    }
    return false
  }

}
