package sed

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._

class BufferTest {
  
  val zeroString: String = ""
  val unitString: String = "u"
  val message1: String = "The quick brown fox jumps over the lazy dog"
  val message2: String = "Now Is The Time For All Good Men To Come To The Aid Of Parity"
  val word: String = "aardvark"
  
  @Test def test_ar_00 {
    val buf: Buffer = new Buffer(zeroString)
    buf.ar()
    assertEquals(0, buf.getCursor)
  }
  @Test def test_ar_01 {
    val buf: Buffer = new Buffer(unitString)
    buf.ar()
    assertEquals(1, buf.getCursor)
  }
  @Test def test_ar_02 {
    val buf: Buffer = new Buffer(message1)
    buf.ar()
    assertEquals(1, buf.getCursor)
  }
  @Test def test_al_03 {
    val buf: Buffer = new Buffer(zeroString)
    buf.al()
    assertEquals(0, buf.getCursor)
  }
  @Test def test_al_04 {
    val buf: Buffer = new Buffer(unitString)
    buf.al()
    assertEquals(0, buf.getCursor)
  }
  @Test def test_al_05 {
    val buf: Buffer = new Buffer(message1)
    buf.al()
    assertEquals(0, buf.getCursor)
  }
  @Test def test_tr_06 {
    val buf: Buffer = new Buffer(zeroString)
    buf.tr()
    assertEquals(0, buf.getCursor)
  }
  @Test def test_tr_07 {
    val buf: Buffer = new Buffer(unitString)
    buf.tr()
    assertEquals(1, buf.getCursor)
  }
  @Test def test_tr_08 {
    val buf: Buffer = new Buffer(message1)
    buf.tr()
    assertEquals(message1.length, buf.getCursor)
  }
  @Test def test_tl_09 {
    val buf: Buffer = new Buffer(zeroString)
    buf.tl()
    assertEquals(0, buf.getCursor)
  }
  @Test def test_tl_10 {
    val buf: Buffer = new Buffer(unitString)
    buf.tl()
    assertEquals(0, buf.getCursor)
  }
  @Test def test_tl_11 {
    val buf: Buffer = new Buffer(message1)
    buf.tl()
    assertEquals(0, buf.getCursor)
  }
  @Test def test_go_12 {
    val buf: Buffer = new Buffer(zeroString)
    buf.go(0)
    assertEquals(0, buf.getCursor)
  }
  @Test def test_go_13 {
    val buf: Buffer = new Buffer(zeroString)
    buf.go(1)
    assertEquals(0, buf.getCursor)
  }
  @Test def test_go_14 {
    val buf: Buffer = new Buffer(unitString)
    buf.go(0)
    assertEquals(0, buf.getCursor)
  }
  @Test def test_go_15 {
    val buf: Buffer = new Buffer(unitString)
    buf.go(1)
    assertEquals(1, buf.getCursor)
  }
  @Test def test_go_16 {
    val buf: Buffer = new Buffer(unitString)
    buf.go(2)
    assertEquals(1, buf.getCursor)
  }
  @Test def test_go_17 {
    val buf: Buffer = new Buffer(message1)
    buf.go(-1)
    assertEquals(0, buf.getCursor)
  }
  @Test def test_go_18 {
    val buf: Buffer = new Buffer(message1)
    buf.go(0)
    assertEquals(0, buf.getCursor)
  }
  @Test def test_go_19 {
    val buf: Buffer = new Buffer(message1)
    buf.go(1)
    assertEquals(1, buf.getCursor)
  }
  @Test def test_go_20 {
    val buf: Buffer = new Buffer(message1)
    buf.go(message1.length-1)
    assertEquals(message1.length-1, buf.getCursor)
  }
  @Test def test_go_21 {
    val buf: Buffer = new Buffer(message1)
    buf.go(message1.length)
    assertEquals(message1.length, buf.getCursor)
  }
  @Test def test_go_22 {
    val buf: Buffer = new Buffer(message1)
    buf.go(message1.length+1)
    assertEquals(message1.length, buf.getCursor)
  }
  @Test def test_dr_23 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    go(4); dr
    assertEquals(4, getMarker)
  }
  @Test def test_dr_24 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    go(4); dr ; tr
    assertEquals(4, getMarker)
  }
  @Test def test_da_25 {
    val buf: Buffer = new Buffer(zeroString)
    buf.da()
    assertEquals(0, buf.getCursor)
    assertEquals(0, buf.getMarker)
  }
  @Test def test_da_26 {
    val buf: Buffer = new Buffer(unitString)
    buf.da()
    assertEquals(0, buf.getMarker)
    assertEquals(1, buf.getCursor)
  }
  @Test def test_da_27 {
    val buf: Buffer = new Buffer(message1)
    buf.da()
    assertEquals(0, buf.getMarker)
    assertEquals(message1.length, buf.getCursor)
  }
  @Test def test_es_28 {
    val buf: Buffer = new Buffer(zeroString)
    buf.es("")
    assertEquals(0, buf.getCursor)
    assertEquals(0, buf.getMarker)
    assertEquals(zeroString, buf.getString)
  }
  @Test def test_es_29 {
    val buf: Buffer = new Buffer(zeroString)
    buf.es("z")
    assertEquals(1, buf.getCursor)
    assertEquals(0, buf.getMarker)
    assertEquals("z", buf.getString)
  }
  @Test def test_es_30 {
    val buf: Buffer = new Buffer(unitString)
    buf.es("")
    assertEquals(0, buf.getCursor)
    assertEquals(0, buf.getMarker)
    assertEquals(unitString, buf.getString)
  }
  @Test def test_es_31 {
    val buf: Buffer = new Buffer(unitString)
    buf.es("z")
    assertEquals(1, buf.getCursor)
    assertEquals(0, buf.getMarker)
    assertEquals("zu", buf.getString)
  }
  @Test def test_es_32 {
    val buf: Buffer = new Buffer(unitString)
    buf.ar();
    buf.es("z")
    assertEquals(2, buf.getCursor)
    assertEquals(0, buf.getMarker)
    assertEquals("uz", buf.getString)
  }
  @Test def test_es_33 {
    val buf: Buffer = new Buffer(word)
    import buf._
    go(4); es("")
    assertEquals(4, getCursor)
    assertEquals(0, getMarker)
    assertEquals(word, getString)
  }
  @Test def test_es_34 {
    val buf: Buffer = new Buffer(word)
    import buf._
    go(4); es("XX")
    assertEquals(6, buf.getCursor)
    assertEquals(0, buf.getMarker)
    assertEquals("aardXXvark", getString)
  }
  @Test def test_es_35 {
    val buf: Buffer = new Buffer(word)
    import buf._
    tr; es("XX")
    assertEquals(word.length+2, getCursor)
    assertEquals(0, getMarker)
    assertEquals("aardvarkXX", getString)
  }
  @Test def test_xc_36 {
    val buf: Buffer = new Buffer(word)
    import buf._
    go(2); dr; xc
    assertEquals(2, getCursor)
    assertEquals(2, getMarker)
    assertEquals(word, getString)
    assertEquals("", getPaste)
  }
  @Test def test_xc_37 {
    val buf: Buffer = new Buffer(word)
    import buf._
    go(4); dr; ar; ar; ar; xc
    assertEquals(7, getCursor)
    assertEquals(4, getMarker)
    assertEquals(word, getString)
    assertEquals("var", getPaste)
  }
  @Test def test_xc_38 {
    val buf: Buffer = new Buffer(word)
    import buf._
    dr; rpt(4)(ar); xc
    assertEquals(4, getCursor)
    assertEquals(0, getMarker)
    assertEquals(word, getString)
    assertEquals("aard", getPaste)
  }
  @Test def test_xc_39 {
    val buf: Buffer = new Buffer(word)
    import buf._
    da; xc
    assertEquals(word.length, getCursor)
    assertEquals(0, getMarker)
    assertEquals(word, getString)
    assertEquals(word, getPaste)
  }
  @Test def test_xc_40 {
    val buf: Buffer = new Buffer(word)
    import buf._
    tr; dr; al; al; xc
    assertEquals(word.length-2, getCursor)
    assertEquals(word.length, getMarker)
    assertEquals(word, getString)
    assertEquals("rk", getPaste)
  }
  @Test def test_xd_41 {
    val buf: Buffer = new Buffer(word)
    import buf._
    go(2); dr; xd
    assertEquals(2, getCursor)
    assertEquals(2, getMarker)
    assertEquals("aardvark", getString)
    assertEquals("", getPaste)
  }
  @Test def test_xd_42 {
    val buf: Buffer = new Buffer(word)
    import buf._
    go(4); dr; ar; ar; ar; xd
    assertEquals(4, getCursor)
    assertEquals(4, getMarker)
    assertEquals("aardk", getString)
    assertEquals("var", getPaste)
  }
  @Test def test_xd_43 {
    val buf: Buffer = new Buffer(word)
    import buf._
    dr; rpt(4)(ar); xd
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("vark", getString)
    assertEquals("aard", getPaste)
  }
  @Test def test_xd_44 {
    val buf: Buffer = new Buffer(word)
    import buf._
    da; xd
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("", getString)
    assertEquals("aardvark", getPaste)
  }
  @Test def test_xd_45 {
    val buf: Buffer = new Buffer(word)
    import buf._
    tr; dr; al; al; xd
    assertEquals(word.length-2, getCursor)
    assertEquals(word.length-2, getMarker)
    assertEquals("aardva", getString)
    assertEquals("rk", getPaste)
  }
  @Test def test_xp_46 {
    val buf: Buffer = new Buffer(zeroString)
    import buf._
    xp()
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("", getString)
    assertEquals("", getPaste)
  }
  @Test def test_xp_47 {
    val buf: Buffer = new Buffer(unitString)
    import buf._
    da ; xd ; xp
    assertEquals(1, getCursor)
    assertEquals(0, getMarker)
    assertEquals(unitString, getString)
    assertEquals(unitString, getPaste)
  }
  @Test def test_xp_48 {
    val buf: Buffer = new Buffer(word)
    import buf._
    da ; xd ; xp
    assertEquals(word.length, getCursor)
    assertEquals(0, getMarker)
    assertEquals(word, getString)
    assertEquals(word, getPaste)
  }
  @Test def test_xp_49 {
    val buf: Buffer = new Buffer(word)
    import buf._
    da ; xd ; xp ; xp
    assertEquals(word.length*2, getCursor)
    assertEquals(0, getMarker)
    assertEquals(word+word, getString)
    assertEquals(word, getPaste)
  }
  @Test def test_ee_50 {
    val buf: Buffer = new Buffer(zeroString)
    import buf._
    ee
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("", getString)
    assertEquals("", getPaste)
  }
  @Test def test_ee_51 {
    val buf: Buffer = new Buffer(unitString)
    import buf._
    ee
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("", getString)
    assertEquals("", getPaste)
  }
  @Test def test_ee_52 {
    val buf: Buffer = new Buffer(word)
    import buf._
    ee
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals(word.tail, getString)
    assertEquals("", getPaste)
  }
  @Test def test_ee_53 {
    val buf: Buffer = new Buffer(word)
    import buf._
    rpt(20)(ee)
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("", getString)
    assertEquals("", getPaste)
  }
  @Test def test_ee_54 {
    val buf: Buffer = new Buffer(word)
    import buf._
    tr; dr; al ; al ; ee
    assertEquals(word.length-2, getCursor)
    assertEquals(word.length-1, getMarker)
    assertEquals("aardvak", getString)
    assertEquals("", getPaste)
  }
  
  @Test def test_ed_55 {
    val buf: Buffer = new Buffer(zeroString)
    import buf._
    ed
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("", getString)
    assertEquals("", getPaste)
  }
  @Test def test_ed_56 {
    val buf: Buffer = new Buffer(unitString)
    import buf._
    ar; ed
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("", getString)
    assertEquals("", getPaste)
  }
  @Test def test_ed_57 {
    val buf: Buffer = new Buffer(word)
    import buf._
    tr; ed
    assertEquals(word.length-1, getCursor)
    assertEquals(0, getMarker)
    assertEquals(word.init, getString)
    assertEquals("", getPaste)
  }
  @Test def test_ed_58 {
    val buf: Buffer = new Buffer(word)
    import buf._
    tr; rpt(20)(ed)
    assertEquals(0, getCursor)
    assertEquals(0, getMarker)
    assertEquals("", getString)
    assertEquals("", getPaste)
  }
  @Test def test_ed_59 {
    val buf: Buffer = new Buffer(word)
    import buf._
    tr; dr; al ; al ; ed
    assertEquals(word.length-3, getCursor)
    assertEquals(word.length-1, getMarker)
    assertEquals("aardvrk", getString)
    assertEquals("", getPaste)
  }
  @Test def test_cc_60 {
    val buf: Buffer = new Buffer(zeroString)
    import buf._
    da; cc
    assertEquals("", getString)
    assertEquals("", getPaste)
  }
  @Test def test_cc_61 {
    val buf: Buffer = new Buffer(word)
    import buf._
    da; cc
    assertEquals("AARDVARK", getString)
  }
  @Test def test_cc_62 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    go(15); dr; tr; xd; da; cc
    assertEquals("nOW iS tHE tIME", getString)
    assertEquals(" For All Good Men To Come To The Aid Of Parity", getPaste)
  }
  @Test def test_cc_63 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    go(15); dr; tr; xd; dr; go(7); ee; ee; ee; ee; tl; cc
    assertEquals(0, getCursor)
    assertEquals(11, getMarker)
    assertEquals("nOW iS tIME", getString)
    assertEquals(" For All Good Men To Come To The Aid Of Parity", getPaste)
  }
  @Test def test_cc_64 {
    val buf: Buffer = new Buffer(word)
    import buf._
    ar; ar; xd; dr; tr; cc; xp
    assertEquals(8, getCursor)
    assertEquals(0, getMarker)
    assertEquals("RDVARKaa", getString)
    assertEquals("aa", getPaste)
  }
  @Test def test_sc_65 {
    val buf: Buffer = new Buffer(word)
    import buf._
    dr; tr; sc('a','X')
    assertEquals("XXrdvXrk", getString)
    assertEquals("", getPaste)
  }
  @Test def test_sc_66 {
    val buf: Buffer = new Buffer(word)
    import buf._
    dr; tr; xc; sc('a','X')
    assertEquals("XXrdvXrk", getString)
    assertEquals("aardvark", getPaste)
  }
  @Test def test_sc_67 {
    val buf: Buffer = new Buffer(word)
    import buf._
    da; xc; sc('a','X'); sc('r','Y')
    assertEquals("XXYdvXYk", getString)
    assertEquals("aardvark", getPaste)
  }
  @Test def test_sc_68 {
    val buf: Buffer = new Buffer(word)
    import buf._
    ar; ar; dr; ar; ar; ar; xd; da; sc('r','Z'); sc('d','D')
    assertEquals("aaaZk", getString)
    assertEquals("rdv", getPaste)
  }
  @Test def test_sc_69 {
    val buf: Buffer = new Buffer(word)
    import buf._
    ar; dr; ar; sc('a','X'); sc('X','Y')
    assertEquals("aYrdvark", getString)
    assertEquals("", getPaste)
  }
  @Test def test_sc_70 {
    val buf: Buffer = new Buffer(word)
    import buf._
    dr; ar; ar; ar; xd; da; sc('a','X'); xp
    assertEquals("dvXrkaar", getString)
    assertEquals("aar", getPaste)
  }
  @Test def test_dd_71 {
    val buf: Buffer = new Buffer(word)
    import buf._
    dd
    assertEquals("aardvark", getString)
    assertEquals("", getPaste)
  }
  @Test def test_dd_72 {
    val buf: Buffer = new Buffer(word)
    import buf._
    da; dd
    assertEquals("ardvk", getString)
    assertEquals("", getPaste)
  }
  @Test def test_dd_73 {
    val buf: Buffer = new Buffer(word)
    import buf._
    ar; dr; tr; dd
    assertEquals("aardvk", getString)
    assertEquals("", getPaste)
  }
  @Test def test_dd_74 {
    val buf: Buffer = new Buffer(word)
    import buf._
    da; xc; dd; tr; es(" = "); xp
    assertEquals("ardvk = aardvark", getString)
    assertEquals("aardvark", getPaste)
  }
  @Test def test_dd_75 {
    val buf: Buffer = new Buffer(message1)
    import buf._
    da; dd
    assertEquals("The quickbrownfxjmpsvtlazydg", getString)
  }
  @Test def test_dd_76 {
    val buf: Buffer = new Buffer("..........")
    import buf._
    da; dd
    assertEquals(".", getString)
  }
  @Test def test_ff_77 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    ff('N')
    assertEquals(0, getCursor)
  }
  @Test def test_ff_78 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    ff('I')
    assertEquals(4, getCursor)
  }
  @Test def test_ff_79 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    ff('y')
    assertEquals(60, getCursor)
  }
  @Test def test_ff_80 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    ff('z')
    assertEquals(61, getCursor)
  }
  @Test def test_ff_81 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    while(ff(' ')) ee
    assertEquals("NowIsTheTimeForAllGoodMenToComeToTheAidOfParity", getString)
  }
  @Test def test_ff_82 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    while(ff(_ == ' ')) ee
    assertEquals("NowIsTheTimeForAllGoodMenToComeToTheAidOfParity", getString)
  }
  @Test def test_ff_83 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    while(ff(_ == ' ')) ee; tl; while(ff(_.isUpper)) {dr; ar; cc}
    assertEquals("nowisthetimeforallgoodmentocometotheaidofparity", getString)
  }
  @Test def test_ff_84 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    while(ff(_ => false))
    assertEquals(message2, getString)
    assertEquals(61, getCursor)
  }
  @Test def test_ff_85 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    while(ff(_ => true)) ee
    assertEquals("", getString)
    assertEquals(0, getCursor)
  }
  @Test def test_ff_86 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    while(ff(c => c.isSpaceChar || c.isLower)) ee
    assertEquals("NITTFAGMTCTTAOP", getString)
    assertEquals(15, getCursor)
  }
  @Test def test_fb_87 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    fb('N')
    assertEquals(0, getCursor)
  }
  @Test def test_fb_88 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    fb('I')
    assertEquals(0, getCursor)
  }
  @Test def test_fb_89 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    tr; fb('y')
    assertEquals(60, getCursor)
  }
  @Test def test_fb_90 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    fb('z')
    assertEquals(0, getCursor)
  }
  @Test def test_fb_91 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    tr; while(fb(' ')) ee
    assertEquals("NowIsTheTimeForAllGoodMenToComeToTheAidOfParity", getString)
  }
  @Test def test_fb_92 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    tr; while(fb(_ == ' ')) ee
    assertEquals("NowIsTheTimeForAllGoodMenToComeToTheAidOfParity", getString)
  }
  @Test def test_fb_93 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    tr; while(fb(_ == ' ')) ee; tr; while(fb(_.isLower)) {dr; ar; cc; al}
    assertEquals("NOWISTHETIMEFORALLGOODMENTOCOMETOTHEAIDOFPARITY", getString)
  }
  @Test def test_fb_94 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    tr; while(fb(_ => false))
    assertEquals(message2, getString)
    assertEquals(0, getCursor)
  }
  @Test def test_fb_95 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    tr; while(fb(_ => true)) ee
    assertEquals("", getString)
    assertEquals(0, getCursor)
  }
  @Test def test_fb_96 {
    val buf: Buffer = new Buffer(message2)
    import buf._
    tr; while(fb(c => c.isSpaceChar || c.isLower)) ee
    assertEquals("NITTFAGMTCTTAOP", getString)
    assertEquals(0, getCursor)
  }
  @Test def test_97 {
    val buf: Buffer = new Buffer(message1)
    import buf._
    ff('q'); dr; ff('b'); xd 
    assertEquals("The brown fox jumps over the lazy dog", getString)
    assertEquals(4, getMarker)
    assertEquals(4, getCursor)
  }
  @Test def test_98 {
    val buf: Buffer = new Buffer(message1)
    import buf._
    dr; tr; es("."); tl; ff('.'); xd
    assertEquals(".", getString)
    assertEquals(message1, getPaste)
    assertEquals(0, getMarker)
    assertEquals(0, getCursor)
  }
  @Test def test_99 {
    val buf: Buffer = new Buffer(message1)
    import buf._
    tr; es("."); dr; tl; ff('.'); xd
    assertEquals(message1, getString)
    assertEquals(".", getPaste)
    assertEquals(43, getMarker)
    assertEquals(43, getCursor)
  }
}
