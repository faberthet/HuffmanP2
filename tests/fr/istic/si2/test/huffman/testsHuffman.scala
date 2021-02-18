//berthet fabrice L1 IE G4

package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

import fr.istic.si2.huffman._

class TestsHuffman {

  /**
   * Un arbre à utiliser dans tous les tests
   */
  val h: Huffman = Noeud(1.00 , Noeud(0.57, Feuille(0.25, 'a'),Noeud(0.32,Feuille(0.18,'c'),Feuille(0.14,'d'))),
      Noeud(0.43,Feuille(0.21,'b'),Noeud(0.22,Noeud(0.13,Feuille(0.07,'f'),Feuille(0.06,'g')),Feuille(0.09,'e'))))

   val l:List[Bit]=Zero::Zero::Nil
   val m:List[Bit]=One::One::One::Nil
   val lc:List[Char]= 'a'::'b'::'c'::Nil
   val c2:List[Bit]= Zero::Zero::One::Zero::Nil
   val c3:List[Bit]= Zero::Zero::One::Zero::Zero::One::Zero::Nil
   val c4:List[Bit]= Zero::Zero::One::Nil
      
 
  /**
   * Test d'encodage d'un caractère V0
   */
  @Test
  def testEncodeSymbolv0() {
    
    assertTrue(encodeSymbol('a',h)== Some(Zero::Zero::Nil))
    assertTrue(encodeSymbol('e',h)== Some(m))
    assertFalse(encodeSymbol('c',h)== Some(m))
    assertTrue(encodeSymbol('r',h)== None)
    //fail() 
  }
  
  /**
   * Test de decodage d'une liste de Bits
   */
  @Test
  def testDecodeSymbolv0() {
    
    assertTrue(decodeSymbolv0(h,l)==Some('a'))
    assertFalse(decodeSymbolv0(h,m)==Some('a'))
    assertTrue(decodeSymbolv0(h,Zero::Zero::Zero::Nil)==None)
    assertTrue(decodeSymbolv0(h,Zero::Nil)==None)
  }
  
  /**
   * Test d'encodage d'une liste de caractère en liste de bits
   */
  @Test
  def testEncodeList() {
    
    assertTrue(encodeList(lc,h)==c3)
    assertFalse(encodeList(lc,h)==c4)
    assertTrue(encodeList('a'::'r'::Nil,h)==l)
    assertTrue(encodeList('r'::Nil,h)==Nil)
  }
  
  
  /**
   * Test d'encodage d'une chaine en liste de bits
   */
  @Test
  def testEncode() {
    
    assertTrue(encode("abc",h)==c3)
    assertFalse(encode("abc",h)==c2)
    assertTrue(encode("ar",h)==Zero::Zero::Nil)
    assertTrue(encode("kr",h)==Nil)
  }
  
  
  /**
   * Test de decodage d'une liste de Bits en un tuple(option[char], list[Bits])
   */
  @Test
  def testDecodeSymbol() {
    
    assertTrue(decodeSymbol(h,c2)==(Some('a'),One::Zero::Nil))
    assertFalse(decodeSymbol(h,c2)==(Some('b'),One::Zero::Nil))
    assertTrue(decodeSymbol(h,c3)==(Some('a'),One::Zero::Zero::One::Zero::Nil))
    assertTrue(decodeSymbol(h,Nil)==(None,Nil))
  }
  
  /**
   * Test de decodage d'une liste de Bits en chaine de caractère
   */
  @Test
  def testDecode() {
    
    assertTrue(decode(c3,h)=="abc")
    assertTrue(decode(c4,h)=="a")
    assertFalse(decode(c4,h)=="abc")
    assertTrue(decode(Nil,h)=="")
    
  }
   
}
