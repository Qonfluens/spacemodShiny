ref_ocsge <- data.frame(
  code_cs = c(
    "CS 1.1.1.1",
    "CS 1.1.1.2",
    "CS 1.1.2.1",
    "CS 1.1.2.2",
    "CS 1.2.1",
    "CS 1.2.2",
    "CS 1.2.3",
    "CS 2.1.1.1",
    "CS 2.1.1.2",
    "CS 2.1.1.3",
    "CS 2.1.2",
    "CS 2.1.3",
    "CS 2.2.1",
    "CS 2.2.2"
  ),
  nomenclature = c(
    "Zones bâties",
    "Zones non bâties",
    "Matériaux minéraux",
    "Matériaux composites",
    "Sols nus",
    "Surfaces d'eau",
    "Névés et glaciers",
    "Feuillus",
    "Conifères",
    "Mixte",
    "Formations arbustives, sous-arbrisseaux",
    "Autres formations ligneuses",
    "Formations herbacées",
    "Autres formations non ligneuses"
  ),
  couleur = c(
    "#FF1493", # CS 1.1.1.1 - Rose vif
    "#FF9999", # CS 1.1.1.2 - Saumon
    "#FFFF99", # CS 1.1.2.1 - Jaune pâle
    "#8B4513", # CS 1.1.2.2 - Marron
    "#C0C0C0", # CS 1.2.1   - Gris
    "#00BFFF", # CS 1.2.2   - Bleu cyan
    "#C4FFFF", # CS 1.2.3   - Bleu très pâle/Menthe
    "#7FFF00", # CS 2.1.1.1 - Vert chartreuse
    "#006400", # CS 2.1.1.2 - Vert foncé
    "#6B8E23", # CS 2.1.1.3 - Vert olive
    "#90EE90", # CS 2.1.2   - Vert clair
    "#FF8C00", # CS 2.1.3   - Orange
    "#CCFF00", # CS 2.2.1   - Jaune-Vert
    "#F0FFF0"  # CS 2.2.2   - Vert très pâle
  ),
  stringsAsFactors = FALSE
)

# Aperçu
print(ref_ocsge)
