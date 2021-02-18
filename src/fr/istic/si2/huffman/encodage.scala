//berthet fabrice L1 IE G4

package fr.istic.si2.huffman

object Encodage {
  
  
  
  
  

  /**
   * @param c un caractère
   * @param h un arbre de Huffman
   * @return l'encodage de c, selon h, s'il existe
   */
  def encodeSymbol(c: Char, h: Huffman): Option[List[Bit]] = {
    
    
    h match {
      
      case Feuille(_,d) => if(d==c){Some(Nil)} else{None}
      
      case Noeud(_,g,d) => encodeSymbol(c,g) match{ 
                                             case Some(t) => Some(Zero::t)
                                             case None => encodeSymbol(c,d) match {
                                                                            case Some(t) => Some(One::t)                                                
                                                                            case None => None
                                                                            }
                                                          
                                            }
        
        
      }
      
    }
      
    

  /**
   * @param l une liste de caractères
   * @param h un arbre de Huffman
   * @return la séquence de bits correspondants à
   *         l'encodage selon h des éléments de l, s'il a réussi.
   *         Les caractères pour lesquels l'encodage est impossible sont oubliés
   */ 
  def encodeList(l: List[Char], h: Huffman): List[Bit] = {
     
    l match{
      
      case Nil => Nil
      case a::t => (encodeSymbol(a,h)) match {
        
                                       case None => encodeList(t,h)
                                       case Some(Nil) => encodeList(t,h)
                                       case Some(b) => b++encodeList(t,h)
        
                                       }
      
    }
      
  }

  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  def encode(s: String, h: Huffman): List[Bit] = {
    
    encodeList(s.toList,h) 
      
  }

}