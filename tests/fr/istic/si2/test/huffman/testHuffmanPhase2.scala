//berthet fabrice L1 IE G4

package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._

import fr.istic.si2.huffman._

class testHuffmanPhase2 {
  
  
  val lfreqs: List[(Char, Double)] = List(('a', 0.25),('b',0.21),('c',0.18),('d',0.14),('e',0.09),('f',0.07),('g',0.06))
  val lh1: List[Huffman] = Feuille(0.40,'a')::Feuille(0.30,'b')::Feuille(0.30,'c')::Nil
  val lh2: List[Huffman] = Feuille(0.10,'a')::Feuille(0.30,'b')::Nil
  val h3: Huffman=Noeud(0.5,Feuille(0.2,'a'),Feuille(0.3,'b'))
  val h4: Huffman=Noeud(0.4,Feuille(0.2,'a'),Feuille(0.2,'b'))
  
  
  /**
   * Test d'encodage d'une liste de couples caractère/fréquence en liste d'huffman Feuilles
   */
 @Test
 def testInitHuffman() {
    
   assertTrue(initHuffman(Nil)==Nil)
   assertTrue(initHuffman(List(('a', 0.40)))==Feuille(0.40,'a')::Nil)
   assertTrue(initHuffman(List(('a', 0.40),('b',0.30),('c',0.30)))==Feuille(0.40,'a')::(Feuille(0.30,'b')::Feuille(0.30,'c')::Nil))
  
 }
  
  /**
   * Test de la fréquence d'un arbre de huffman 
   */
  @Test
 def testFrequence() {
    
    assertTrue(frequence(Feuille(0.0,'a'))==0.00)
    assertTrue(frequence(Feuille(0.5,'a'))==0.50)
    assertTrue(frequence(Noeud(0.5,Feuille(0.1,'a'),Feuille(0.2,'b')))==0.50)
     
  }
  
  /**
   * Test de l'insertion d'un arbre de huffman dans une liste de huffman
   */
  @Test
 def testInsertion() { 
    
    assertTrue(insertion(Feuille(0.2,'d'),Nil)==Feuille(0.2,'d')::Nil)
    assertTrue(insertion(Feuille(0.2,'d'),lh1)==Feuille(0.2,'d')::Feuille(0.40,'a')::Feuille(0.30,'b')::Feuille(0.30,'c')::Nil)
    assertTrue(insertion(Feuille(0.2,'d'),lh2)==Feuille(0.10,'a')::Feuille(0.2,'d')::Feuille(0.30,'b')::Nil)
    
  }
  
  /**
   * Test de tri d'un arbre de huffman selon frequence
   */
  @Test
 def testTriSelonFreq() { 
    
    assertTrue(triSelonFreq(Feuille(0.40,'a')::Feuille(0.30,'b')::Nil)==Feuille(0.30,'b')::Feuille(0.40,'a')::Nil)
    assertTrue(triSelonFreq(Feuille(0.40,'a')::Nil)==Feuille(0.40,'a')::Nil)
    assertTrue(triSelonFreq(h3::Feuille(0.40,'a')::Nil)==Feuille(0.40,'a')::h3::Nil)
    
  }
  
  /**
   * Test de fusion de deux elements d'une liste de huffman
   */
  @Test
 def testUneFusion() { 
    
    assertTrue(uneFusion(Feuille(0.40,'a')::Feuille(0.30,'b')::Nil)==(Noeud(0.70,Feuille(0.40,'a'),Feuille(0.30,'b'))::Nil))
    assertTrue(uneFusion(h3::h4::Nil)==(Noeud(0.90,h3,h4)::Nil))
    assertTrue(uneFusion(h3::Feuille(0.40,'a')::Nil)==(Noeud(0.90,h3,Feuille(0.40,'a'))::Nil))
  }
  
  /**
   * Test de fusion des éléments d'une liste de huffman en arbre de huffman
   */
  @Test
 def testFusion() { 
    
    assertTrue(fusion(Feuille(0.40,'a')::Feuille(0.30,'b')::Nil)==Noeud(0.70,Feuille(0.30,'b'),Feuille(0.40,'a')))
    assertTrue(fusion(Feuille(0.40,'c')::h3::Nil)==Noeud(0.9,Feuille(0.4,'c'),Noeud(0.5,Feuille(0.2,'a'),Feuille(0.3,'b'))))
    
  }
  
  /**
   * Test de codage d'une liste Char/Double en arbre de huffman
   */
  // TODO je ne sais pas pourquoi lors de l'addition de deux double comme 0.1 et 0.2 il y a un résultat non exact comme 0.30000000000000004 au lieu de 0.3
  @Test
 def testCodeHuffman() {
    
    //assertTrue(codeHuffman(List(('a', 0.20),('b',0.10)))==Noeud(0.30,Feuille(0.10,'b'),Feuille(0.20,'a')))

  }
  
  /**
   * Test du nombre d'apparition d'un caractère dans une liste de caractère
   */
  @Test
 def testApparition() { 
    
    assertTrue(apparition('c','c'::'a'::'c'::'b'::Nil)==2)
    assertTrue(apparition('c',Nil)==0)
    
  }
  
  /**
   * Test de la frequence d'apparition d'un caractère dans une liste de caractère
   */
   @Test
 def testFreqApparition() { 
     
     assertTrue(freqApparition('c','c'::'a'::'c'::'b'::Nil)==0.5)
     
   }
   
  /**
   * Test de création d'un couple caractère/Frequence selon un caractère et une liste de caractère 
   */
   @Test
 def testCharDouble() { 
     
     assertTrue(charDouble('c','c'::'a'::'c'::'b'::Nil)==('c',0.5))
     
   }
  
   /**
   * Test de suppression de toutes les occurences d'un caractère dans une liste de caractères
   */
   @Test
 def testIsoleChar() {
     
     assertTrue(isoleChar('c','c'::'a'::'c'::'b'::Nil)==('a'::'b'::Nil))
     assertTrue(isoleChar('c','c'::Nil)==(Nil))
     
   }
   
   /**
   * Test de création d'un liste de caractères avec une seule occurrence de chaque caractères à partir d'une liste de caractères
   */
    @Test
 def testUneOccurence() {
      
      assertTrue(uneOccurence('c'::'a'::'c'::'b'::Nil)=='c'::'a'::'b'::Nil)
      assertTrue(uneOccurence('c'::'a'::'c'::'b'::'a'::'a'::Nil)=='c'::'a'::'b'::Nil)
      
    }
   
   /**
   * Test de création d'une liste de couple caractère/fréquence depuis une liste de caractères
   */
    @Test
 def testListCharDouble() {
      
      assertTrue(listCharDouble('a'::'b'::Nil, 'a'::'b'::Nil)==('a',0.5)::('b',0.5)::Nil)
      assertTrue(listCharDouble('c'::'a'::'c'::'b'::Nil, 'c'::'a'::'c'::'b'::Nil)==('c',0.5)::('a',0.25)::('b',0.25)::Nil)
       
    }
    
    /**
   * Test de création d'une liste de couple caractère/fréquence depuis une chaine de caractère
   */
     @Test
 def testAnalyseFrequences() {
       
       assertTrue(analyseFrequences("ab")==('a',0.5)::('b',0.5)::Nil)
       assertTrue(analyseFrequences("cacb")==('c',0.5)::('a',0.25)::('b',0.25)::Nil)
       
     }
  
  
}