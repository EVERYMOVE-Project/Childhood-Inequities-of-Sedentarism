#### ENSE 2001 ####

# Define start and end positions
start <- c(1, 5, 10, 12, 14, 17, 18, 19, 21, 24, 28, 31, 32, 33, 35, 
           37, 38, 40, 42, 92, 93, 97, 102, 104, 107, 122, 128, 130, 
           153, 154, 156, 179, 180, 182, 183, 227,228, 230, 232, 234, 
           237, 239, 242, 245, 247, 249, 251, 253, 263,264, 273, 274, 
           275, 278, 280, 282, 285, 287, 362, 365, 382, 385,388, 422, 
           439, 441, 443, 446, 448, 450, 453, 455, 456, 459, 461,463)

end <- c( 4, 9, 11, 13, 16, 17, 18, 20, 23, 27, 28, 31, 32, 34, 36, 37, 
          39, 41, 91, 92, 96, 101, 103,106, 110, 127, 129, 152, 153, 
          155, 178, 181, 181, 226, 226, 229, 231, 233, 236, 238, 241, 
          244, 246, 248, 250, 252, 262, 263, 272,273, 274, 277, 279, 
          281, 284, 286, 293, 363, 378, 384, 387, 388,438, 440, 442, 
          445, 447, 449, 452, 454, 455, 458, 460, 462, 464)

names_menores2001 <- c("n_estudio", "n_cuestionario", "ccaa", "provincia", "municipio", "tam_habitat", "area_metro", 
                       "distrito", "seccion", "n_entrevistador", "submuestra_1", "cuestionario_infantil",
                       "p32_precod", "edad_meses_1", "edad_anos_1", "p37_precod", "edad_entrevistado",
                       "n_personas_vivienda", "p2_p4_familia", "p5_precod", "p5a_edad_dedicacion",
                       "p5b_p6a_precod", "p7_precod", "p8_p10_precod", "p10a_dolencia_10dias",
                       "p12_p13_precod", "p13a_dias_lim_act_libre", "p13a_precod_rest", "p14_precod",
                       "p14a_dias_act_principal", "p14b_precod", "p15_precod", "p15a_dias_en_cama",
                       "p16_precod", "p16a_b_precod", "p17_precod", "p17a_anos", "p17a_meses",
                       "p18_n_veces_consulta", "p18b_precod", "p18c_especialidad", "p18d_tiempo_domicilio",
                       "p18e_tiempo_espera", "p18f_p19_precod", "p19a_dentista_veces", "p20_anos",
                       "p20_meses", "p21_precod", "p22_precod", "p23_precod", "p23a_precod", "p24a_n_hospital",
                       "p24b_dias_ingreso", "p24c_d_precod", "p24e_espera_meses", "p24f_p25_precod",
                       "p25a_urgencias_veces", "p25b_p27_precod", "p38_horas_sueno", "p40_p43a_precod",
                       "p45_peso", "p46_estatura", "p46a_precod", "p52_p57_precod", "p57a_estudios_entrevistado",
                       "p58_p58a_precod", "p59_ocupacion", "p60_p60a_precod", "p61_rama_empresa",
                       "p62_p64_precod", "p64a_estudios_cabeza", "p65_precod", "p66_ocupacion_cabeza",
                       "p67_p67a_precod", "p67b_rama_empresa", "p69_p70_precod")

# Calculate widths
width_menores2001 <- end - start + 1

menores2001 <- read_fwf("~/UAH/PhD Documents/INEdatos/2001ENSE/INFANT2001/INFANT01.txt", col_positions = fwf_widths(widths = width_menores2001, col_names = names_menores2001))
