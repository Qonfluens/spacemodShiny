# On ne charge PAS le raster ici (sinon le pointeur mourra).
# On prépare juste la liste.

DATA_TRY <- list(
  # SF est safe, on peut le garder tel quel
  roi_metaleurop = spacemodR::roi_metaleurop,
  ocsge_metaleurop = spacemodR::ocsge_metaleurop,
  # Pour le raster, on stocke une FONCTION qui le chargera au moment voulu
  get_ground_cd = function() {
    spacemodR::load_raster_extdata("ground_concentration_cd.tif")
  }
)
