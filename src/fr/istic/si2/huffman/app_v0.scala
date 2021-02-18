//berthet fabrice L1 IE G4


package fr.istic.si2.huffman

import scala.io.Source
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

/**
 * Type algébrique simple modélisant les bits (0 ou 1)
 */
sealed trait Bit
case object Zero extends Bit
case object One extends Bit

/**
 * Type algébrique récursif modélisant les arbres de code de Huffman
 */
sealed trait Huffman
case class Feuille(freq: Double, c: Char) extends Huffman
case class Noeud(freq: Double, zero: Huffman, one: Huffman) extends Huffman

/**
 * Application principale V0
 */
object HuffmanApp0 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
 
  val h: Huffman = Noeud(1.00 , Noeud(0.57, Feuille(0.25, 'a'),Noeud(0.32,Feuille(0.18,'c'),Feuille(0.14,'d'))),
      Noeud(0.43,Feuille(0.21,'b'),Noeud(0.22,Noeud(0.13,Feuille(0.07,'f'),Feuille(0.06,'g')),Feuille(0.09,'e'))))

  /**
   * @param l une liste de bits
   * @return la chaîne de caractères où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */
 
  def toString(l: List[Bit]): String = {
    
    l match {
      
      case Nil => ""
      case h::t => if (h==Zero) { "0" + toString(t) } else { "1" + toString(t) }
      
      
    }
    
    
  }
   
  /**
   * affiche un caractère [char] encodé puis décodé selon huffman
   * @param h un arbre de huffman
   */
  def appPrincipalv0( h: Huffman): Unit = {
    
    
    
    //val a:List[Bit]=encodeSymbol(c,h).getOrElse(Nil)
    //val b:Char= decodeSymbolv0(h,a).getOrElse(' ')
    
  
    for(c <-'a' to 'm'){
    
      val a:List[Bit]=encodeSymbol(c,h).getOrElse(Nil)
      val b:Char= decodeSymbolv0(h,a).getOrElse(' ')
        
        
      if(encodeSymbol(c, h)==None){
        
        println(c + " Symbol inexistant dans l'arbre")
        
      }else{
      
        print (c + " "); print(a);  print(" "); print(toString(a)); print(" " + b); println()
        
      }//fin if
      
      
   }//fin for
    
  }// fin appPrincipalv0

  
   val l:List[Bit]=Zero::Zero::Nil
   val m:List[Bit]=One::One::One::Nil
      
      //print(decodeSymbolv0(h,m))
      //print(encodeSymbol('b', h))
      //print(toString(m))
   
   appPrincipalv0(h)
   
   
}