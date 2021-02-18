//berthet fabrice L1 IE G4

package fr.istic.si2.huffman
import java.math

object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = {
    
    l match {
      
      case Nil => Nil
      case h::t => Feuille(h._2,h._1)::initHuffman(t)
        
    }
     
  }

  /**
   * @param h un arbres de Huffman
   * @return la frequence d'une feuille ou d'un noeud
   */
  def frequence(h:Huffman): Double ={
    
    h match {
      
      case Feuille(a,_) => a
      case Noeud(b,_,_) => b
    } 
  }
  
  /**
   * @param f un arbre de huffman
   * @param l une liste d'arbres de Huffman
   * @return une liste avec f insérer dans l en respectant un ordre croissant des frequence
   */
  def insertion(f: Huffman, l: List[Huffman]): List[Huffman]= {
  
  l match{
    
    case Nil => List(f)
    case h::t => if (frequence(h)>=frequence(f)){ f::h::t} else{ h::insertion(f,t) }
  } 
}
  
  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  // On pourra s'inspirer du tri par insertion programmé sur les listes d'entiers en TP4
  def triSelonFreq(l: List[Huffman]): List[Huffman] = {
    
    l match{
      
      case Nil => Nil
      case h::t => insertion(h, triSelonFreq(t)) 
    } 
  }
  
  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = {
 
    fusion(initHuffman(freqs))
    
  }

  /**
   * @param l une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
  def uneFusion(l: List[Huffman]): List[Huffman] = {
    
    l match {
     
      case a::b::Nil => Noeud(frequence(a) + frequence(b) ,a,b)::Nil
      case _ => sys.error("cette liste n'a pas 2 elements")
    }
  }

  /**
   * @param l une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  def fusion(l: List[Huffman]): Huffman = {
    
    triSelonFreq(l) match {
      
      case Nil => sys.error("liste vide")
      case a::Nil => a
      case h::i::t => fusion(uneFusion(h::i::Nil)++t)
    }
  }

  /**
   * @param c un caractère
   * @param lc une chaine de caractère
   * @return le nombre d'apparition de c dans lc
   */
  def apparition(c: Char, lc: List[Char]): Double = {
    
    lc match {
      
    case Nil => 0
    case h::t => if(h==c){1 + apparition(c,t)}else{apparition(c,t)}
      
    }
   
  }
  
  /**
   * @param c un caractère
   * @param lc une chaine de caractère
   * @return la fréquence d'apparition de c dans lc
   */
  def freqApparition(c: Char, lc: List[Char]): Double = {
    
    apparition(c,lc)/lc.length
    
  }
  
  /**
   * @param c un caractère
   * @param lc une chaine de caractère
   * @return le couple caractère/frequence d'appartition du caractère dans lc
   */
  def charDouble(c: Char, lc: List[Char]): (Char, Double) = {
    
   (c, freqApparition(c, lc))
      
  }
  
  /**
   * @param c un caractère
   * @param lc une chaine de caractère
   * @return une Liste de caractères otée de toute les apparitions de c
   */
  def isoleChar(c: Char, lc: List[Char]): List[Char] = {
    
    lc match {
      
      case Nil => Nil
      case h::t => if (h!=c){h::isoleChar(c,t)}else{isoleChar(c,t)}
      
    }
  }
  
   /**
   * @param lc une chaine de caractère
   * @return une Liste de caractères avec une seule apparition de chaque caractère de la list lc
   */
  def uneOccurence(lc: List[Char]): List[Char] = {
    
    lc match{
      
      case Nil => Nil
      case h::t => h::uneOccurence(isoleChar(h, lc))

    }
  }
  
  /**
   * @param lc une chaine de caractère
   * @param lc2 une chaine de caractère identique à lc
   * @return une Liste de couple caractère/double correspondant au tableau de frequence de caractère de lc
   */
  def listCharDouble(lc: List[Char], lc2: List[Char]): List[(Char, Double)] = {
  
    uneOccurence(lc) match {
      
      case Nil => Nil
      case h::t => charDouble(h, lc2)::listCharDouble(t, lc2)
      
    }
  }
  
  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  def analyseFrequences(s: String): List[(Char, Double)] = {

   listCharDouble(s.toList, s.toList)
  
  }
  
}