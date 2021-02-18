//berthet fabrice L1 IE G4

package fr.istic.si2.huffman

import scala.io.Source
import java.io.{ File, PrintWriter }
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._

/**
 * Application principale
 */
object HuffmanApp extends App {

  /**
   * Une liste de couples caractère / fréquence d'apparition
   * à utilisée par l'application principale.
   */
  
  
  val lfreqs: List[(Char, Double)] = List(('a', 0.25),('b',0.21),('c',0.18),('d',0.14),('e',0.09),('f',0.07),('g',0.06))
  
  val lfreqs2: List[(Char, Double)] = List(('a', 0.15),('b',0.12),('c',0.10),('d',0.14),('e',0.09),('f',0.07),('g',0.06),('h',0.02),('i',0.05),('j',0.03),
      ('k',0.02),('l',0.05),('m',0.04),('n',0.04),('o',0.02))
      
  /**
   * @param nom le nom d'un fichier
   * @return la chaîne contenue dans le fichier nommé nom
   */
  def lireFichier(nom: String): String = {
    Source.fromFile(nom).getLines.mkString
  }

  /**
   * Ecrit une chaîne de caractères dans un fichier.
   *  Le fichier est écrasé s'il était déjà existant.
   *
   * @param nom le nom du fichier dans lequel on écrit
   * @param contenu la chaîne de caractères à écrire
   */
  def ecrireFichier(nom: String, contenu: String): Unit = {
    val writer = new PrintWriter(new File(nom))
    writer.write(contenu)
    writer.close()
  }

  
  
  /**
   * @param l une liste de bits
   * @return la chaîne de caractères où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */   
 def toString2(l: List[Bit]): String = {
    
    l match {
      
      case Nil => ""
      case h::t => if (h==Zero) { "0" + toString2(t) } else { "1" + toString2(t) }
       
    }
    
  }
  
  /**
   * affiche dans un fichier texte l'encodage d'un texte provenant d'un autre fichier
   * @param freqchar une liste de couple caratctère/Double
   */
  def appPrincipalv2(freqchar: List[(Char,Double)]): Unit = {
    
    println("arbre d'huffman obtenu à partir de la liste de couple caractère/Double prédéfinie:")
    println(codeHuffman(freqchar))
    println()
    println("insérez une chaine caractère:")
    val chaine = io.StdIn.readLine()
    println("arbre créé par analyse de la frequence des caractères:")
    println(codeHuffman(analyseFrequences(chaine)))
    println()
    
    
    println("insérez un nom du fichier contenant un texte à encoder (exemple: fichiers/texte1.txt):")
    val texte = io.StdIn.readLine()
  
    ecrireFichier("fichiers/compression.txt", toString2(encode(lireFichier(texte),codeHuffman(freqchar))))
    println("le texte encodé avec l'arbre prédéfinie est disponible dans HuffmanP2/fichiers/compression.txt")
   
    ecrireFichier("fichiers/compression2.txt", toString2(encode(lireFichier(texte),codeHuffman(analyseFrequences(texte)))))
    println("le texte encodé par analyse de la fréquence de chaque caractère est disponible dans HuffmanP2/fichiers/compression2.txt")
    
  }
  
  
    appPrincipalv2(lfreqs)
  //appPrincipalv2(lfreqs2)
  
  //println(analyseFrequences("aabc"))
  
  //println(codeHuffman(lfreqs))
  
  //println(triSelonFreq(Feuille(0.33,'a')::Feuille(0.17,'b')::Feuille(0.30,'c')::Feuille(0.20,'d')::Nil))
  //println(fusion(Feuille(0.33,'a')::Feuille(0.17,'b')::Feuille(0.30,'c')::Feuille(0.20,'d')::Nil))
  //println(charDouble('a', 'a'::'a'::'b'::'c'::Nil))
  //println(listCharDouble('a'::'a'::'b'::'c'::Nil, 'a'::'a'::'b'::'c'::Nil))
  //println(isoleChar('b', 'a'::'a'::'b'::'c'::' '::Nil))
  //println(uneOccurence('a'::'a'::'b'::'c'::'b'::'b'::'c'::'a'::Nil))
  
 //println(codeHuffman(List(('a', 0.20),('b',0.10))))
  
  
  
}