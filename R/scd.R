#
#
#
# s <- star_schema() |>
#   define_dimension(
#     name = "Where",
#     attributes = "", # todos o resto de los atributos (type_2).
#     natural_key = c("COD_PRO",
#                     "COD_MUN"),
#     type_0 = c("att1", "att2", "att3"),
#     type_1 = c("att4", "att5"),
#     type_3 = c("att6"),
#     type_6 = c("att7")
#   )
#
#
# configure_scd() # define los valores siguientes.
#
# previous = "previous"    # se crea asociado a type_3: att_previous
# historic = "historic"    # se crea asociado a type_6: att_historic
# version = "version"      # campo para guardar la versión
# value_current = "current"
# value_expired = "expired"

