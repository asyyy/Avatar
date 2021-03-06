package machine

object TestToleranceApp extends App {
 
    
   val  l = List( ("Mairie de Rennes","Place de la Mairie"),
                ("Hôtel de ville","Place de la Mairie"),
                ("Théâtre La Paillette","2 Rue du Pré de Bris"),
                ("La Paillette" ,"2, Rue du Pré de Bris"),
                ("Théâtre National de Bretagne", "1 Rue Saint-Hélier"),
                ("TNB1", "Rue Saint-Hélier"),
                ("Théâtre de Bretagne","1, Rue Saint-Hélier"),
                ("Gare SNCF", "19 Place de la Gare"),
                ("Gare","19, Place de la Gare") )
                
    println(tolerance_au_faute.corrige("Mairie de rENNeeeyfuuigigys",l));
}