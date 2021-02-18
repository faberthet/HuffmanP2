//berthet fabrice L1 IE G4

package fr.istic.si2.huffman

import scala.io.Source
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

/**
 * Application principale V1
 */
object HuffmanApp1 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
 
  val h: Huffman = Noeud(1.00 , Noeud(0.57, Feuille(0.25, 'a'),Noeud(0.32,Feuille(0.18,'c'),Feuille(0.14,'d'))),
      Noeud(0.43,Feuille(0.21,'b'),Noeud(0.22,Noeud(0.13,Feuille(0.07,'f'),Feuille(0.06,'g')),Feuille(0.09,'e'))))
      
      
   /**
   * @param l une liste de bits
   * @return la chaîne de caractères où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */   
    def toString1(l: List[Bit]): String = {
    
    l match {
      
      case Nil => ""
      case h::t => if (h==Zero) { "0" + toString1(t) } else { "1" + toString1(t) }
       
    }
    
  }
      

  /**
   * @param s une chaîne de caractères
   * @return la chaîne de 0 et 1 représentant chaque caractère
   *         de s par son encodage sur 16 bits
   */
  def vers16Bits(s: String): String = {
    s.toList.map(c => String.format("%16s", c.toBinaryString).replace(' ', '0')).reduce(_ + _)
  }

  /**
   * affiche la chaine encodée standard et selon l'arbre de huffman
   * @param h un arbre de huffman        
   */
  def appPrincipalv1(h: Huffman): Unit = {

     var encore:Boolean = true
     
     while(encore){
       
        println("chaine à encoder:")
       
        val s = io.StdIn.readLine() 
       
        println("Chaîne encodée standard:")
        println(vers16Bits(s))
        println("Taille (nb Bits):" + vers16Bits(s).length())
            
            if(encode(s, h)==Nil){ 
              
            println(s + ": aucun des caractères présent dans cette chaîne n'existe dans l'arbre")
              
            }else{
            
            println("Chaîne encodée Huffman :")
            println(toString1(encode(s, h)))
            println("Taille (nb Bits):" + toString1(encode(s, h)).length())
            println("Chaîne décodée Huffman :")
            println(decode(encode(s, h), h))
            
            }//fin if
        
        println("encore?(y/n)")
        
        val yn = io.StdIn.readLine()
          
       encore=(yn!="n")
        
     }//fin while
     
  }//fin appprincipalv1
  
  
   val lc:List[Char]= 'a'::'b'::'c'::Nil 
   val c2:List[Bit]= Zero::Zero::One::Zero::Nil
   val c3:List[Bit]= Zero::Zero::One::Zero::Zero::One::Zero::Nil
   val c4:List[Bit]= Zero::Zero::One::Nil
  
  /*println(encodeList(lc,h))
   println(encode("abc", h))
   println(decodeSymbol(h,c2))
   println(decodeSymbol(h,c3))
   println(decode(c3,h))
   println(decode(c4,h))  */
   
   
   appPrincipalv1(h)
   
   
}