# Class que se crea a partir de una star_database y una flat table con dos opciones
# según se quiera hacer con las instancias de los hechos que ya existan (las nuevas no hay problema)
# 1. sustituir las instancias de los hechos existentes con las nuevas que aparezcan
# 2. agregar las instancias de los hechos existentes y las nuevas que aparezcan
# se aplican las transformaciones sobre la flat table
# se actualiza la star_database (hechos y dimensiones)
# se generan nuevas instancias en dimensiones y hechos
# se generan actualizaciones sobre instancias existentes en los hechos
#
# Adicionalmente guardar y restaurar una star_database de una BDR.
