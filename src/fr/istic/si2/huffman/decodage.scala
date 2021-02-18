//berthet fabrice L1 IE G4

package fr.istic.si2.huffman

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return caractère correspondant au décodage de l selon h
   */
  def decodeSymbolv0(h: Huffman, l: List[Bit]): Option[Char] = {
    
    (h,l) match{
      
      case (Feuille(_,c),Nil) => Some(c)
      
      case (Feuille(_,_),h::t) => None
      
      case (Noeud(_,a,b),Zero::t) => decodeSymbolv0(a,t)
      
      case (Noeud(_,a,b),One::t) => decodeSymbolv0(b,t)
      
      case (Noeud(_,_,_),Nil) => None
      
    }
    
  }
      
      
  

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage de l selon h
   *         - deuxième composante : la liste des bits restant à décoder après avoir suivi l dans h.
   */
  def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
    
   (h,l) match{
         
      case (Feuille(_,c),Nil) => (Some(c),Nil)
      
      case (Feuille(_,c),t) => (Some(c),t)
      
      case (Noeud(_,a,b),Zero::t) => decodeSymbol(a,t)
      
      case (Noeud(_,a,b),One::t) => decodeSymbol(b,t)
      
      case (Noeud(_,_,_),Nil) => (None,Nil)
      
    }
  
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h
   */
  def decode(l: List[Bit], h: Huffman): String = {
     
    decodeSymbol(h, l) match{
        
      case (Some(c), Nil) => c + ""
      case (Some(c),t) => c + decode(t,h)
      case (None,t) => "" 
      
    }      
 }
  
    
}