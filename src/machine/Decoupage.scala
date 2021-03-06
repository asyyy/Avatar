package machine

trait AnalyseT {
  /**
   * A partir d'une phrase en un seul string, on créé une liste de tout les mots que l'on compare à la base
   * de donnée.
   
   @param s la phrase
   @param l la base de donnée
   @return la liste des mots communs
   */
   def AnalysePhrase(s : String, lang:List[String], salut:List[String]) : List[String]
}

trait DataBaseTrait {
  /**
   * Analyse le texte et construit rend la liste de couple [clé,valeur]
   * @param base la base de données au format: clé;valeur
   * @return la liste (clé,valeur)
   */
  def constructionBase(base : List[String]) : List[(String,String)]
  
}


trait Tolerance {

     /**
     * 
     */
  def ajoute_Char_alafin(s:String,i:Int) : String
  
     /**
     * 
     */
  def taille_egale(s:String, m:String) : String 
   /**
     * 
     */
  def hamming (s:String , m:String): Int 
   
     /**
     * 
     */
   def correction(s: String): String 
  
     /**
     * 
     */
   def corrige (s:String, couple: List[(String,String)] ) : String 
  }