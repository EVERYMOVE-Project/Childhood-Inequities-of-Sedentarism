/**********************************************************************************************************************						
Instituto Nacional de Estadística (INE) www.ine.es						
***********************************************************************************************************************						
						
DESCRIPCIÓN:						
Este programa genera un fichero SAS con formatos, partiendo de un fichero sin ellos.						
						
Consta de las siguientes partes:						
	* 1. Definir la librería de trabajo --> Libname					
	* 2. Definición de formatos --> PROC FORMAT					
	* 3. Vincular formatos a la base de datos --> PASO data					
						
 Entrada:                                                           						
     - Fichero SAS sin formatos: 	 EESEadultos_2009.sas7bdat					
 Salida:                                                           						
     - Fichero SAS con formatos: 	 EESEadultos_2009_conFormato.sas7bdat					
						
Donde:						
	* Operación: Encuesta Europea de Salud en España. Cuestionario adultos					
	* Periodo: 2009					
						
************************************************************************************************************************/						
						
/*1) Definir la librería de trabajo: introducir el directorio que desee como librería						
(se da como ejemplo 'C:\Mis resultados'), y copiar en ese directorio el fichero sas "EESEadultos_2009.sas7bdat"*/						
						
libname ROutput 'C:\Mis resultados';	

options fmtsearch = (ROutput ROutput.cat1);

* 2) DEFINICIÓN DE FORMATOS;
PROC FORMAT LIBRARY=ROutput.cat1;
value $TCCAA

"01"="Andalucía"
"02"="Aragón"
"03"="Asturias, Principado de"
"04"="Balears, Illes"
"05"="Canarias"
"06"="Cantabria"
"07"="Castilla y León"
"08"="Castilla - La Mancha"
"09"="Cataluña"
"10"="Comunitat Valenciana"
"11"="Extremadura"
"12"="Galicia"
"13"="Madrid, Comunidad de"
"14"="Murcia, Región de"
"15"="Navarra, Comunidad Foral de"
"16"="País Vasco"
"17"="Rioja, La"
"18"="Ceuta"
"19"="Melilla"
;
value $TMUNIC

"1"="Menor o igual a 10.000 habitantes"
"2"="De 10.001 a 50.000 habitantes"
"3"="De 50.001 a 100.000 habitantes"
"4"="De 100.001 a 400.000 habitantes"
"5"="Mayor o igual a 400.001 habitantes"
;
value $TSEXO

"1"="Hombre"
"2"="Mujer"
;
value $TSINO

"1"="Sí"
"6"="No"
"8"="No sabe"
"9"="No contesta"
;
value $T16SINO

"1"="Sí"
"6"="No"
;
value $T1PROXY

"1"="La persona seleccionada está ingresada en un sanatorio, hospital,…a causa de una enfermedad"
"2"="La persona seleccionada está incapacitada para contestar por discapacidad, enfermedad grave,…"
"3"="La persona seleccionada está incapacitada para contestar por causa del idioma"
;
value $T5PROXY

"1"="Cónyuge o pareja"
"2"="Hijo/a"
"3"="Padre/Madre"
"4"="Hermano/Hermana"
"5"="Otros familiares"
"6"="Servicios sociales"
"7"="Voluntarios"
"8"="Otros"
;
value $TNAC

"1"="España"
"2"="Extranjero"
"8"="No sabe"
"9"="No contesta"
;
value $TECIVIL

"1"="Soltero/a"
"2"="Casado/a"
"3"="Viudo/a"
"4"="Separado legalmente"
"5"="Divorciado/a"
"8"="No sabe"
"9"="No contesta"
;
value $TESTUD

"01"="No sabe leer o escribir (ISCED 0)"
"02"="Estudios primarios incompletos (ISCED 1)"
"03"="Educación primaria o equivalentes (ISCED 1)"
"04"="Educación secundaria de primera etapa (ISCED 2)"
"05"="Estudios de Bachillerato (ISCED 3)"
"06"="Enseñanzas profesionales de grado medio o equivalentes (ISCED 3)"
"07"="Enseñanzas profesionales de grado superior o equivalentes (ISCED 5B)"
"08"="Estudios universitarios de 1 y 2 ciclo o equivalentes(ISCED 5A)"
"09"="Doctorado o equivalente (ISCED 6)"
"98"="No sabe"
"99"="No contesta"
;
value $TACTPRF

"1"="Asalariado (a sueldo, comisión, jornal...)"
"2"="Empresario con asalariados"
"3"="Empresario sin asalariados"
"4"="Miembro de una cooperativa"
"5"="Trabajador en el negocio familiar"
"6"="Otra situación"
"8"="No sabe"
"9"="No contesta"
;
value $TCONTRT

"1"="Contrato laboral indefinido ( fijo, fijo discontinuo)"
"2"="Contrato laboral de duración limitada"
"8"="No sabe"
"9"="No contesta"
;
value $TOCUPC

"1"="Tiempo completo"
"2"="Tiempo parcial"
"8"="No sabe"
"9"="No contesta"
;
value $TSALUDP

"1"="Muy bueno"
"2"="Bueno"
"3"="Regular"
"4"="Malo"
"5"="Muy malo"
;
value $TGRADO

"1"="Gravemente limitado"
"2"="Limitado pero no gravemente"
"3"="Nada limitado"
;
value $T_1PL

"1"="Sí"
"2"="No, nunca"
"3"="Soy ciego o no puedo ver en absoluto"
"8"="No sabe"
"9"="No contesta"
;
value $T_2PL

"1"="Sí, sin dificultad"
"2"="Con alguna dificultad"
"3"="Con dificultad severa"
"4"="No puede hacerlo"
"8"="No sabe"
"9"="No contesta"
;
value $T_4PL

"1"="Sí"
"2"="No"
"3"="Soy sordo profundo"
"8"="No sabe"
"9"="No contesta"
;
value $T1_1PC

"1"="No tengo ninguna dificultad"
"2"="Sí, alguna dificultad"
"3"="Sí, dificultad severa"
"4"="No puedo hacerlo por mí mismo"
"8"="No sabe"
"9"="No contesta"
;
value $T_DFCTD

"1"="No tengo ninguna dificultad"
"2"="Sí, alguna dificultad"
"3"="Sí, mucha dificultad"
"4"="No puedo hacerlo por mí mismo"
"8"="No sabe"
"9"="No contesta"
;
value $T_MOTV

"1"="Principalmente, por el estado de salud, discapacidad o edad avanzada"
"2"="Principalmente por otras razones (nunca intenté hacerlo, etc)"
"8"="No sabe"
"9"="No contesta"
;
value $T_1SF

"1"="Leve"
"2"="Moderada"
"3"="Severa"
"4"="Extrema"
"8"="No sabe"
"9"="No contesta"
;
value $T_2SF

"1"="Siempre"
"2"="Casi siempre"
"3"="Algunas veces"
"4"="Sólo alguna vez"
"5"="Nunca"
"8"="No sabe"
"9"="No contesta"
;
value $T_SINOC

"1"="Sí, al menos en una ocasión"
"6"="No, en ninguna ocasión"
"8"="No sabe"
"9"="No contesta"
;
value $T_RAZON

"1"="No me lo podía permitir (demasiado caro o no cubierto por el seguro)"
"2"="Lista de espera, u otras razones debidas al hospital"
"3"="No disponía de tiempo debido al trabajo, al cuidado de los niños o de otras personas"
"4"="Demasiado lejos para viajar, sin medios de transporte"
"5"="Miedo a la cirujía/tratamiento"
"6"="Otras razones"
"8"="No sabe"
"9"="No contesta"
;
value $T_ULTVS

"1"="En las últimas cuatro semanas"
"2"="Entre 4 semanas y 12 meses"
"3"="Hace 12 meses o más"
"4"="Nunca"
;
value $T_DONDE

"1"="En una consulta externa de un hospital o ambulatorio"
"2"="En un servicio de urgencias de un hospital"
"3"="En una consulta de médico particular o de una sociedad"
"4"="En su lugar de trabajo o su centro de estudios"
"5"="Otro lugar"
"8"="No sabe"
"9"="No contesta"
;
value $T_MTIVO

"01"="No me lo podía permitir (demasiado caro o no cubierto por el seguro)"
"02"="Lista de espera, no tenía volante"
"03"="No disponía de tiempo debido al trabajo, al cuidado de los niños o de otras personas"
"04"="Demasiado lejos para viajar/ sin medios de transporte"
"05"="Miedo al médico, hospitales, exploraciones médicas, tratamiento"
"06"="Quise esperar y ver si el problema mejoraba por sí solo"
"07"="No conocía a ningún buen especialista"
"08"="Otras razones"
"98"="No sabe"
"99"="No contesta"
;
value $T1PERIO

"1"="Durante este año"
"2"="El año pasado"
"3"="Antes del año pasado"
"8"="No sabe"
"9"="No contesta"
;
value $T2PERIO

"1"="En los últimos 12 meses"
"2"="Hace más de un año pero no más de 5 años"
"3"="Hace más de 5 años"
"8"="No sabe"
"9"="No contesta"
;
value $T3PERIO

"1"="Hace 3 años o menos de 3 años"
"2"="Hace más de tres años"
"8"="No sabe"
"9"="No contesta"
;
value $T4PERIO

"1"="Hace tres años o menos de tres años"
"2"="Hace más de tres años, pero no más de cinco años"
"3"="Hace más de cinco años"
"8"="No sabe"
"9"="No contesta"
;
value $T5PERIO

"1"="En los últimos 12 meses"
"2"="Hace más de 1 año, pero no más de 2"
"3"="Hace más de 2 años, pero no más de 3 años"
"4"="Hace más de 3 años"
"8"="No sabe"
"9"="No contesta"
;
value $T_IMPMM

"0"="No imputado"
"1"="Mes imputado"
"2"="Año imputado"
"3"="Mes y año imputado"
;
value $T_MTCIT

"1"="Porque tenía molestias"
"2"="Porque se lo recomendaron en la consulta de medicina de familia"
"3"="Porque fue a la consulta de ginecología"
"4"="Porque le citaron desde su CCAA o ayuntamiento para un programa de detección precoz de cáncer"
"5"="Por otra razón de tipo médico"
"6"="Por otro tipo de razones no médicas"
"8"="No sabe"
"9"="No contesta"
;
value $T_SATIS

"1"="Muy satisfecho"
"2"="Bastante satisfecho"
"3"="Ni satisfecho ni insatisfecho"
"4"="Bastante insatisfecho"
"5"="Muy insatisfecho"
"8"="No sabe"
"9"="No contesta"
;
value $T_FREC

"1"="Dos o más veces al día"
"2"="Una vez al día"
"3"="Menos de una vez al día, pero al menos 4 veces a la semana"
"4"="Menos de 4 veces, pero al menos 1 vez a la semana"
"5"="Menos de 1 vez a la semana"
"6"="Nunca"
"8"="No sabe"
"9"="No contesta"
;
value $T1EXPSC

"1"="Muy expuesto"
"2"="Algo expuesto"
"3"="No expuesto"
"8"="No sabe"
"9"="No contesta"
;
value $T_PERSN

"1"="Ninguna"
"2"="1 o 2"
"3"="De 3 a 5"
"4"="Más de 5"
"8"="No sabe"
"9"="No contesta"
;
value $T_FUMA

"1"="Sí fuma diariamente"
"2"="Sí fuma, pero no diariamente"
"3"="No fuma actualmente, pero ha fumado antes"
"4"="No fuma, ni ha fumado nunca de manera habitual"
"9"="No contesta"
;
value $T2EXPSC

"1"="Nunca o casi nunca"
"2"="Menos de una hora al día"
"3"="Entre 1 y 5 horas al día"
"4"="Más de 5 horas al día"
"9"="No contesta"
;
value $T_2FREC

"1"="Nunca"
"2"="Una vez al mes o menos"
"3"="De 2 a 4 veces al mes"
"4"="De 2 a 3 veces a la semana"
"5"="De 4 a 6 veces a la semana"
"6"="Todos los días"
"9"="No contesta"
;
value $T_3FREC

"1"="Nunca"
"2"="Menos de una vez al mes"
"3"="Mensualmente"
"4"="Semanalmente"
"5"="A diario o casi a diario"
"9"="No contesta"
;
value $T_INTVL

"01"="Menos de 550 euros"
"02"="De 550 a menos de 850 euros"
"03"="De 850 a menos de 1.150 euros"
"04"="De 1.150 a menos de 1.400 euros"
"05"="De 1.400 a menos de 1.700 euros"
"06"="De 1.700 a menos de 2.000 euros"
"07"="De 2.000 a menos de 2.400 euros"
"08"="De 2.400 a menos de 2.900 euros"
"09"="De 2.900 a menos de 3.600 euros"
"10"="De 3.600 euros en adelante"
"98"="No sabe"
;
value $T_IMC

"1"="Peso insuficiente"
"2"="Normopeso"
"3"="Sobrepeso"
"4"="Obesidad"
"9"="No contesta"
;
value $TPAIS

"004"="Afganistán"
"008"="Albania"
"012"="Argelia"
"016"="Samoa Americana"
"020"="Andorra"
"024"="Angola"
"028"="Antigua y Barbuda"
"031"="Azerbaiyán"
"032"="Argentina"
"036"="Australia"
"040"="Austria"
"044"="Bahamas"
"048"="Bahrein"
"050"="Bangladesh"
"051"="Armenia"
"052"="Barbados"
"056"="Bélgica"
"060"="Bermudas"
"064"="Bhután"
"068"="Bolivia"
"070"="Bosnia-Herzegovina"
"072"="Botswana"
"076"="Brasil"
"084"="Belice"
"090"="Islas Salomón"
"092"="Islas Vírgenes Británicas"
"096"="Brunei Darussalam"
"100"="Bulgaria"
"104"="Myanmar"
"108"="Burundi"
"112"="Bielorrusia"
"116"="Camboya"
"120"="Camerún"
"124"="Canadá"
"132"="Cabo Verde"
"136"="Islas Caimán"
"140"="República Centroafricana"
"144"="Sri Lanka"
"148"="Chad"
"152"="Chile"
"156"="China"
"170"="Colombia"
"174"="Comoras"
"175"="Mayotte"
"178"="Congo"
"180"="República Democrática del Congo"
"184"="Islas Cook"
"188"="Costa Rica"
"191"="Croacia"
"192"="Cuba"
"196"="Chipre"
"203"="República Checa"
"204"="Benin"
"208"="Dinamarca"
"212"="Dominica"
"214"="República Dominicana"
"218"="Ecuador"
"222"="El Salvador"
"226"="Guinea Ecuatorial"
"231"="Etiopía"
"232"="Eritrea"
"233"="Estonia"
"234"="Islas Feroé"
"238"="Islas Falkland (Malvinas)"
"242"="Fidji"
"246"="Finlandia"
"248"="Islas Aland"
"250"="Francia"
"254"="Guayana Francesa"
"258"="Polinesia Francesa"
"262"="Djibuti"
"266"="Gabón"
"268"="Georgia"
"270"="Gambia"
"275"="Territorios Ocupados de Palestina"
"276"="Alemania"
"288"="Ghana"
"292"="Gibraltar"
"296"="Kiribati"
"300"="Grecia"
"304"="Groenlandia"
"308"="Granada"
"312"="Guadalupe"
"316"="Guam"
"320"="Guatemala"
"324"="Guinea"
"328"="Guyana"
"332"="Haití"
"336"="Vaticano, Santa Sede"
"340"="Honduras"
"344"="Hong-Kong, Región Administrativa Especial de China"
"348"="Hungría"
"352"="Islandia"
"356"="India"
"360"="Indonesia"
"364"="Irán (República Islámica de)"
"368"="Irak"
"372"="Irlanda"
"376"="Israel"
"380"="Italia"
"384"="Costa de Marfil"
"388"="Jamaica"
"392"="Japón"
"398"="Kazajstán"
"400"="Jordania"
"404"="Kenia"
"408"="República Pop. Democ. de Corea"
"410"="República de Corea"
"414"="Kuwait"
"417"="Kirghizistan"
"418"="República Democrática Popular de Laos"
"422"="Líbano"
"426"="Lesotho"
"428"="Letonia"
"430"="Libéria"
"434"="República Árabe deLibia"
"438"="Liechtenstein"
"440"="Lituania"
"442"="Luxemburgo"
"446"="Macao Región Administrativa Especial de China"
"450"="Madagascar"
"454"="Malawi"
"458"="Malasia"
"462"="Maldivas"
"466"="Malí"
"470"="Malta"
"474"="Martinica"
"478"="Mauritania"
"480"="Mauricio"
"484"="México"
"492"="Mónaco"
"496"="Mongolia"
"498"="República de Moldavia"
"500"="Montserrat"
"504"="Marruecos"
"508"="Mozambique"
"512"="Omán"
"516"="Namibia"
"520"="Naurú"
"524"="Nepal"
"528"="Países Bajos"
"530"="Antillas Neerlandesas"
"533"="Aruba"
"540"="Nueva Caledonia"
"548"="Vanuatu"
"554"="Nueva Zelanda"
"558"="Nicaragua"
"562"="Níger"
"566"="Nigeria"
"570"="Niue"
"574"="Islas Norfolk"
"578"="Noruega"
"580"="Islas Marianas del Norte"
"583"="Micronesia (Estados Federados de )"
"584"="Islas Marshall"
"585"="Palau"
"586"="Pakistán"
"591"="Panamá"
"598"="Papua Nueva Guinea"
"600"="Paraguay"
"604"="Perú"
"608"="Filipinas"
"612"="Pitcairn"
"616"="Polonia"
"620"="Portugal"
"624"="Guinea-Bissau"
"626"="Timor Oriental"
"630"="Puerto Rico"
"634"="Qatar"
"638"="Reunión"
"642"="Rumania"
"643"="Federación Rusa"
"646"="Rwanda"
"654"="Santa Elena"
"659"="Saint Kitts y Nevis"
"660"="Anguila"
"662"="Santa Lucía"
"666"="San Pedro y Miquelón"
"670"="San Vicente y las Granadinas"
"674"="San Marino"
"678"="Santo Tomé y Príncipe"
"682"="Arabia Saudita"
"686"="Senegal"
"690"="Seychelles"
"694"="Sierra Leona"
"702"="Singapur"
"703"="Eslovaquia"
"704"="Vietnam"
"705"="Eslovenia"
"706"="Somalia"
"710"="Africa del Sur"
"716"="Zimbabwe"
"724"="España"
"732"="Sahara Occidental"
"736"="Sudán"
"740"="Suriname"
"744"="Svalbard e Islas de Jan Mayen"
"748"="Swazilandia"
"752"="Suecia"
"756"="Suiza"
"760"="Siria"
"762"="Tayikistan"
"764"="Tailandia"
"768"="Togo"
"772"="Tokelau"
"776"="Tonga"
"780"="Trinidad y Tobago"
"784"="Emiratos Arabes Unidos"
"788"="Túnez"
"792"="Turquía"
"795"="Turkmenistán"
"796"="Islas Turcas y Caicos"
"798"="Tuvalu"
"800"="Uganda"
"804"="Ucrania"
"807"="República de Macedonia, antigua Yugoslavia"
"818"="Egipto"
"826"="Reino Unido de Gran Bretaña e Irlanda del Norte"
"830"="Islas del Canal de la Mancha"
"833"="Islas de Man"
"834"="República Unida de Tanzania"
"840"="Estados Unidos de América"
"850"="Islas Vírgenes de los EEUU"
"854"="Burkina Fasso"
"858"="Uruguay"
"860"="Uzbekistán"
"862"="Venezuela"
"876"="Islas Wallis y Futura"
"882"="Samoa"
"887"="Yemen"
"891"="Serbia y Montenegro"
"894"="Zambia"
"999"="No contesta"
;
value $T_ISCO

"01"="Fuerzas Armadas"
"11"="Miembros del Poder Ejecutivo y de los Cuerpos Legislativos y personal directivo de la Administración Pública"
"12"="Directores de empresa"
"13"="Gerentes de empresas"
"21"="Profesionales de las ciencias físicas, químicas y matemáticas y de la ingeniería"
"22"="Profesionales de las ciencias biológicas, la medicina y la salud"
"23"="Profesionales de la enseñanza"
"24"="Otros profesionales científicos e intelectuales"
"31"="Técnicos y profesionales de nivel medio de las ciencias físicas y químicas, la ingeniería y afines"
"32"="Técnicos y profesionales de nivel medio de las ciencias biológicas, la medicina y la salud"
"33"="Maestros e instructores de nivel medio"
"34"="Otros técnicos y profesionales de nivel medio"
"41"="Oficinistas"
"42"="Empleados en trato directo con el público"
"51"="Trabajadores de los servicios personales y de los servicios de protección y seguridad"
"52"="Modelos, vendedores y demostradores"
"61"="Agricultores y trabajadores calificados de explotaciones agropecuarias, forestales y pesqueras con destino al mercado"
"62"="Trabajadores agropecuarios y pesqueros de subsistencia"
"71"="Oficiales y operarios de las industrias extractivas y de la construcción"
"72"="Oficiales y operarios de la metalurgia, la construcción mecánica y afines"
"73"="Mecánicos de precisión,artesanos, operarios de las artes gráficas y afines"
"74"="Otros oficiales, operarios y artesanos de artes mecánicas y de otros oficios"
"81"="Operadores de instalaciones fijas y afines"
"82"="Operadores de máquinas y montadores"
"83"="Conductores de vehículos y operadores de equipos pesados móviles"
"91"="Trabajadores no calificados de ventas y servicios"
"92"="Peones agropecuarios, forestales, pesqueros y afines"
"93"="Peones de la minería, la construcción, la industria manufacturera y el transporte"
"00"="No contesta"
;
value $T_NACE

"01"="Agricultura, ganadería, caza y servicios relacionados con las mismas"
"02"="Silvicultura y explotación forestal"
"03"="Pesca y acuicultura"
"05"="Extracción de antracita, hulla y lignito"
"06"="Extracción de crudo de petróleo y gas natural"
"07"="Extracción de minerales metálicos"
"08"="Otras industrias extractivas"
"09"="Actividades de apoyo a las industrias extractivas"
"10"="Industria de la alimentación"
"11"="Fabricación de bebidas"
"12"="Industria del tabaco"
"13"="Industria textil"
"14"="Confección de prendas de vestir"
"15"="Industria del cuero y del calzado"
"16"="Industria de la madera y del corcho, excepto muebles; cestería y espartería"
"17"="Industria del papel"
"18"="Artes gráficas y reproducción de soportes grabados"
"19"="Coquerías y refino de petróleo"
"20"="Industria química"
"21"="Fabricación de productos farmacéuticos"
"22"="Fabricación de productos de caucho y plásticos"
"23"="Fabricación de otros productos minerales no metálicos"
"24"="Metalurgia; fabricación de productos de hierro, acero y ferroaleaciones"
"25"="Fabricación de productos metálicos, excepto maquinaria y equipo"
"26"="Fabricación de productos informáticos, electrónicos y ópticos"
"27"="Fabricación de material y equipo eléctrico"
"28"="Fabricación de maquinaria y equipo n.c.o.p."
"29"="Fabricación de vehículos de motor, remolques y semirremolques"
"30"="Fabricación de otro material de transporte"
"31"="Fabricación de muebles"
"32"="Otras industrias manufactureras"
"33"="Reparación e instalación de maquinaria y equipo"
"35"="Suministro de energía eléctrica, gas, vapor y aire acondicionado"
"36"="Captación, depuración y distribución de agua"
"37"="Recogida y tratamiento de aguas residuales"
"38"="Recogida, tratamiento y eliminación de residuos; valorización"
"39"="Actividades de descontaminación y otros servicios de gestión de residuos"
"41"="Construcción de edificios"
"42"="Ingeniería civil"
"43"="Actividades de construcción especializada"
"45"="Venta y reparación de vehículos de motor y motocicletas"
"46"="Comercio al por mayor e intermediarios del comercio, excepto de vehículos de motor y motocicletas"
"47"="Comercio al por menor, excepto de vehículos de motor y motocicletas"
"49"="Transporte terrestre y por tubería"
"50"="Transporte marítimo y por vías navegables interiores"
"51"="Transporte aéreo"
"52"="Almacenamiento y actividades anexas al transporte"
"53"="Actividades postales y de correos"
"55"="Servicios de alojamiento"
"56"="Servicios de comidas y bebidas"
"58"="Edición"
"59"="Actividades cinematográficas, de vídeo y de programas de televisión, grabación de sonido y edición musical"
"60"="Actividades de programación y emisión de radio y televisión"
"61"="Telecomunicaciones"
"62"="Programación, consultoría y otras actividades relacionadas con la informática"
"63"="Servicios de información"
"64"="Servicios financieros, excepto seguros y fondos de pensiones"
"65"="Seguros, reaseguros y fondos de pensiones, excepto Seguridad Social obligatoria"
"66"="Actividades auxiliares a los servicios financieros y a los seguros"
"68"="Actividades inmobiliarias"
"69"="Actividades jurídicas y de contabilidad"
"70"="Actividades de las sedes centrales; actividades de consultoría de gestión empresarial"
"71"="Servicios técnicos de arquitectura e ingeniería; ensayos y análisis técnicos"
"72"="Investigación y desarrollo"
"73"="Publicidad y estudios de mercado"
"74"="Otras actividades profesionales, científicas y técnicas"
"75"="Actividades veterinarias"
"77"="Actividades de alquiler"
"78"="Actividades relacionadas con el empleo"
"79"="Actividades de agencias de viajes, operadores turísticos, servicios de reservas y actividades relacionadas con los mismos"
"80"="Actividades de seguridad e investigación"
"81"="Servicios a edificios y actividades de jardinería"
"82"="Actividades administrativas de oficina y otras actividades auxiliares a las empresas"
"84"="Administración Pública y Defensa; Seguridad Social obligatoria"
"85"="Educación"
"86"="Actividades sanitarias"
"87"="Asistencia en establecimientos residenciales"
"88"="Actividades de servicios sociales sin alojamiento"
"90"="Actividades de creación, artísticas y espectáculos"
"91"="Actividades de bibliotecas, archivos, museos y otras actividades culturales"
"92"="Actividades de juegos de azar y apuestas"
"93"="Actividades deportivas, recreativas y de entretenimiento"
"94"="Actividades asociativas"
"95"="Reparación de ordenadores, efectos personales y artículos de uso doméstico"
"96"="Otros servicios personales"
"97"="Actividades de los hogares como empleadores de personal doméstico"
"98"="Actividades de los hogares como productores de bienes y servicios para uso propio"
"99"="Actividades de organizaciones y organismos extraterritoriales"
"00"="No contesta"
;
value N_1DIG

8="No sabe"
9="No contesta"
;
value N_2DIG

98="No sabe"
99="No contesta"
;
value N_02DIG

00="Código pareja no miembro del hogar"
99="No contesta"
;
value N_3DIG

998="No sabe"
999="No contesta"
;
value N_4DIG

9998="No sabe"
9999="No contesta"
;
value N_5DIG

99998="No sabe"
99999="No contesta"
;


* 3) VINCULAR FORMATOS A LA BASE DE DATOS;
data ROutput.EESEadulto_2009_ConFormato;
	set ROutput.EESEadulto_2009;

FORMAT CCAA $TCCAA.;
FORMAT TMUNI $TMUNIC.;
FORMAT SEXO $TSEXO.;
FORMAT HH_PROXY_0 $T16SINO.;
FORMAT HH_PROXY_1 $T1PROXY.;
FORMAT HH_PROXY_2 $T16SINO.;
FORMAT HH_PROXY_5 $T5PROXY.;
FORMAT HH9_1 $TNAC.;
FORMAT HH9_2 $TPAIS.;
FORMAT HH10_1a $T16SINO.;
FORMAT HH10_1b $T16SINO.;
FORMAT HH10_1c $T16SINO.;
FORMAT HH10_1d $T16SINO.;
FORMAT HH10_2 $TPAIS.;
FORMAT HH11 $TECIVIL.;
FORMAT HH12 $TSINO.;
FORMAT HH12b N_02DIG.;
FORMAT HH13 $TESTUD.;
FORMAT HH14 $TSINO.;
FORMAT HH15a $TACTPRF.;
FORMAT HH15b $TACTPRF.;
FORMAT HH16a $TCONTRT.;
FORMAT HH16b $TCONTRT.;
FORMAT HH17a $TOCUPC.;
FORMAT HH17b $TOCUPC.;
FORMAT HH18a_3 $T_ISCO.;
FORMAT HH18b_3 $T_ISCO.;
FORMAT HH19a_2 $T_NACE.;
FORMAT HH19b_2 $T_NACE.;
FORMAT HS1 $TSALUDP.;
FORMAT HS1b $TSALUDP.;
FORMAT HS2 $TSINO.;
FORMAT HS3 $TGRADO.;
FORMAT HS4_1 $TSINO.;
FORMAT HS5_1 $TSINO.;
FORMAT HS6_1 $TSINO.;
FORMAT HS4_2 $TSINO.;
FORMAT HS5_2 $TSINO.;
FORMAT HS6_2 $TSINO.;
FORMAT HS4_3 $TSINO.;
FORMAT HS5_3 $TSINO.;
FORMAT HS6_3 $TSINO.;
FORMAT HS4_4 $TSINO.;
FORMAT HS5_4 $TSINO.;
FORMAT HS6_4 $TSINO.;
FORMAT HS4_5 $TSINO.;
FORMAT HS5_5 $TSINO.;
FORMAT HS6_5 $TSINO.;
FORMAT HS4_6 $TSINO.;
FORMAT HS5_6 $TSINO.;
FORMAT HS6_6 $TSINO.;
FORMAT HS4_7 $TSINO.;
FORMAT HS5_7 $TSINO.;
FORMAT HS6_7 $TSINO.;
FORMAT HS4_8 $TSINO.;
FORMAT HS5_8 $TSINO.;
FORMAT HS6_8 $TSINO.;
FORMAT HS4_9 $TSINO.;
FORMAT HS5_9 $TSINO.;
FORMAT HS6_9 $TSINO.;
FORMAT HS4_10 $TSINO.;
FORMAT HS5_10 $TSINO.;
FORMAT HS6_10 $TSINO.;
FORMAT HS4_11 $TSINO.;
FORMAT HS5_11 $TSINO.;
FORMAT HS6_11 $TSINO.;
FORMAT HS4_12 $TSINO.;
FORMAT HS5_12 $TSINO.;
FORMAT HS6_12 $TSINO.;
FORMAT HS4_13 $TSINO.;
FORMAT HS5_13 $TSINO.;
FORMAT HS6_13 $TSINO.;
FORMAT HS4_14 $TSINO.;
FORMAT HS5_14 $TSINO.;
FORMAT HS6_14 $TSINO.;
FORMAT HS4_15 $TSINO.;
FORMAT HS5_15 $TSINO.;
FORMAT HS6_15 $TSINO.;
FORMAT HS4_16 $TSINO.;
FORMAT HS5_16 $TSINO.;
FORMAT HS6_16 $TSINO.;
FORMAT HS4_17 $TSINO.;
FORMAT HS5_17 $TSINO.;
FORMAT HS6_17 $TSINO.;
FORMAT HS4_18 $TSINO.;
FORMAT HS5_18 $TSINO.;
FORMAT HS6_18 $TSINO.;
FORMAT HS4_19 $TSINO.;
FORMAT HS5_19 $TSINO.;
FORMAT HS6_19 $TSINO.;
FORMAT HS4_20 $TSINO.;
FORMAT HS5_20 $TSINO.;
FORMAT HS6_20 $TSINO.;
FORMAT HS4_21 $TSINO.;
FORMAT HS5_21 $TSINO.;
FORMAT HS6_21 $TSINO.;
FORMAT HS7_1 $TSINO.;
FORMAT HS8_1 $TSINO.;
FORMAT HS7_2 $TSINO.;
FORMAT HS8_2 $TSINO.;
FORMAT HS7_3 $TSINO.;
FORMAT HS8_3 $TSINO.;
FORMAT HS7_4 $TSINO.;
FORMAT HS8_4 $TSINO.;
FORMAT HS9a $TSINO.;
FORMAT HS9b $TSINO.;
FORMAT HS10 $TSINO.;
FORMAT HS11 N_3DIG.;
FORMAT PL1 $T_1PL.;
FORMAT PL2A $T_2PL.;
FORMAT PL2B $T_2PL.;
FORMAT PL3A $T_2PL.;
FORMAT PL3B $T_2PL.;
FORMAT PL4 $T_4PL.;
FORMAT PL5A $T_2PL.;
FORMAT PL5B $T_2PL.;
FORMAT PL6 $T_2PL.;
FORMAT PL7 $T_2PL.;
FORMAT PL8 $T_2PL.;
FORMAT PL9 $T_2PL.;
FORMAT PL10 $T_2PL.;
FORMAT PL11 $T_2PL.;
FORMAT PC1_1 $T1_1PC.;
FORMAT PC1_2 $T1_1PC.;
FORMAT PC1_3 $T1_1PC.;
FORMAT PC1_4 $T1_1PC.;
FORMAT PC1_5 $T1_1PC.;
FORMAT PC2_1 $TSINO.;
FORMAT PC2_2 $TSINO.;
FORMAT PC2_3 $TSINO.;
FORMAT PC3_1 $TSINO.;
FORMAT PC3_2 $TSINO.;
FORMAT PC3_3 $TSINO.;
FORMAT PC4_1 $TSINO.;
FORMAT PC4_2 $TSINO.;
FORMAT PC4_3 $TSINO.;
FORMAT HA1_1 $T_DFCTD.;
FORMAT HA2_1 $T_MOTV.;
FORMAT HA1_2 $T_DFCTD.;
FORMAT HA2_2 $T_MOTV.;
FORMAT HA1_3 $T_DFCTD.;
FORMAT HA2_3 $T_MOTV.;
FORMAT HA1_4 $T_DFCTD.;
FORMAT HA2_4 $T_MOTV.;
FORMAT HA1_5 $T_DFCTD.;
FORMAT HA2_5 $T_MOTV.;
FORMAT HA1_6 $T_DFCTD.;
FORMAT HA2_6 $T_MOTV.;
FORMAT HA1_7 $T_DFCTD.;
FORMAT HA2_7 $T_MOTV.;
FORMAT HA3_1 $TSINO.;
FORMAT HA3_2 $TSINO.;
FORMAT HA3_3 $TSINO.;
FORMAT HA4_1 $TSINO.;
FORMAT HA4_2 $TSINO.;
FORMAT HA4_3 $TSINO.;
FORMAT HA5_1 $TSINO.;
FORMAT HA5_2 $TSINO.;
FORMAT HA5_3 $TSINO.;
FORMAT SF0 $TSINO.;
FORMAT SF1 $T_1SF.;
FORMAT SF2 $T_2SF.;
FORMAT SF3 $T_2SF.;
FORMAT SF4 $T_2SF.;
FORMAT SF5 $T_2SF.;
FORMAT SF6 $T_2SF.;
FORMAT SF7 $T_2SF.;
FORMAT SF8 $T_2SF.;
FORMAT SF9 $T_2SF.;
FORMAT SF10 $T_2SF.;
FORMAT HC1 $T16SINO.;
FORMAT HC2 N_2DIG.;
FORMAT HC3 N_3DIG.;
FORMAT HC1b $TSINO.;
FORMAT HC2b N_1DIG.;
FORMAT HC3b N_2DIG.;
FORMAT HC3c N_3DIG.;
FORMAT HC4 $TSINO.;
FORMAT HC5 N_3DIG.;
FORMAT HC6 $T_SINOC.;
FORMAT HC7 $T_RAZON.;
FORMAT HC8 $T_ULTVS.;
FORMAT HC9 N_2DIG.;
FORMAT HC10 $T_ULTVS.;
FORMAT HC11 N_2DIG.;
FORMAT HC12 $T_ULTVS.;
FORMAT HC13 N_2DIG.;
FORMAT HC13b $T_DONDE.;
FORMAT HC14 $T_SINOC.;
FORMAT HC15 $T_MTIVO.;
FORMAT HC16_1 $TSINO.;
FORMAT HC16_2 $TSINO.;
FORMAT HC16_3 $TSINO.;
FORMAT HC16_4 $TSINO.;
FORMAT HC16_5 $TSINO.;
FORMAT HC16_6 $TSINO.;
FORMAT HC16_7 $TSINO.;
FORMAT HC16_8 $TSINO.;
FORMAT HC16_9 $TSINO.;
FORMAT HC17_1 $TSINO.;
FORMAT HC17_2 $TSINO.;
FORMAT HC17_3 $TSINO.;
FORMAT HC17_4 $TSINO.;
FORMAT HC18_1 $TSINO.;
FORMAT HC18_2 $TSINO.;
FORMAT HC18_3 $TSINO.;
FORMAT HC18_4 $TSINO.;
FORMAT HC18_5 $TSINO.;
FORMAT MD1a $TSINO.;
FORMAT MD1b $TSINO.;
FORMAT MD2_A $TSINO.;
FORMAT MD2_B $TSINO.;
FORMAT MD2_C $TSINO.;
FORMAT MD2_D $TSINO.;
FORMAT MD2_E $TSINO.;
FORMAT MD2_F $TSINO.;
FORMAT MD2_G $TSINO.;
FORMAT MD2_H $TSINO.;
FORMAT MD2_I $TSINO.;
FORMAT MD2_J $TSINO.;
FORMAT MD2_K $TSINO.;
FORMAT MD2_L $TSINO.;
FORMAT MD2_M $TSINO.;
FORMAT MD2_N $TSINO.;
FORMAT MD2_O $TSINO.;
FORMAT MD2_P $TSINO.;
FORMAT MD2_Q $TSINO.;
FORMAT MD2_R $TSINO.;
FORMAT MD2_S $TSINO.;
FORMAT MD2_T $TSINO.;
FORMAT MD3 $TSINO.;
FORMAT MD4_A $TSINO.;
FORMAT MD4_B $TSINO.;
FORMAT MD4_C $TSINO.;
FORMAT MD4_D $TSINO.;
FORMAT MD4_E $TSINO.;
FORMAT MD4_F $TSINO.;
FORMAT MD4_G $TSINO.;
FORMAT MD4_H $TSINO.;
FORMAT PA1 $TSINO.;
FORMAT PA2 $T1PERIO.;
FORMAT PA3 N_2DIG.;
FORMAT PA4 $TSINO.;
FORMAT PA5 $T2PERIO.;
FORMAT PA6 $TSINO.;
FORMAT PA7 $T2PERIO.;
FORMAT PA8 $TSINO.;
FORMAT PA9 $T2PERIO.;
FORMAT PA10 $TSINO.;
FORMAT PA11 $T3PERIO.;
FORMAT PA11B_1 N_2DIG.;
FORMAT PA11B_2 N_4DIG.;
FORMAT MAMOG_IMP $T_IMPMM.;
FORMAT PA12_01 $T16SINO.;
FORMAT PA12_02 $T16SINO.;
FORMAT PA12_03 $T16SINO.;
FORMAT PA12_04 $T16SINO.;
FORMAT PA12_05 $T16SINO.;
FORMAT PA12_06 $T16SINO.;
FORMAT PA12_07 $T16SINO.;
FORMAT PA12_08 $T16SINO.;
FORMAT PA12_98 $T16SINO.;
FORMAT PA12_99 $T16SINO.;
FORMAT PA13 $TSINO.;
FORMAT PA14 $T4PERIO.;
FORMAT PA14B_1 N_2DIG.;
FORMAT PA14B_2 N_4DIG.;
FORMAT CITOL_IMP $T_IMPMM.;
FORMAT PA15 $T_MTCIT.;
FORMAT PA16 $TSINO.;
FORMAT PA17 $T5PERIO.;
FORMAT SA1_1 $T_SATIS.;
FORMAT SA1_2 $T_SATIS.;
FORMAT SA1_3 $T_SATIS.;
FORMAT SA1_4 $T_SATIS.;
FORMAT SA1_5 $T_SATIS.;
FORMAT BMI1 N_3DIG.;
FORMAT BMI2 N_3DIG.;
FORMAT PE1 N_1DIG.;
FORMAT PE2_1 N_2DIG.;
FORMAT PE2_2 N_2DIG.;
FORMAT PE3 N_1DIG.;
FORMAT PE4_1 N_2DIG.;
FORMAT PE4_2 N_2DIG.;
FORMAT PE5 N_1DIG.;
FORMAT PE6_1 N_2DIG.;
FORMAT PE6_2 N_2DIG.;
FORMAT FV1 $T_FREC.;
FORMAT FV2 $T_FREC.;
FORMAT FV3 $T_FREC.;
FORMAT EN1_1 $T1EXPSC.;
FORMAT EN1_2 $T1EXPSC.;
FORMAT EN1_3 $T1EXPSC.;
FORMAT EN2 $T1EXPSC.;
FORMAT EN3_1 $T1EXPSC.;
FORMAT EN3_2 $T1EXPSC.;
FORMAT EN3_3 $T1EXPSC.;
FORMAT EN3_4 $T1EXPSC.;
FORMAT EN3_5 $T1EXPSC.;
FORMAT EN3_6 $T1EXPSC.;
FORMAT EN3_7 $T1EXPSC.;
FORMAT EN3_8 $T1EXPSC.;
FORMAT EN4 $T_PERSN.;
FORMAT OP1 N_5DIG.;
FORMAT OP2 N_5DIG.;
FORMAT OP3 N_5DIG.;
FORMAT SK1 $T_FUMA.;
FORMAT SK2_1 $TSINO.;
FORMAT SK2_2 $TSINO.;
FORMAT SK2_3 $TSINO.;
FORMAT SK2_4 $TSINO.;
FORMAT SK2_5 $TSINO.;
FORMAT SK3_1 N_2DIG.;
FORMAT SK3_2 N_2DIG.;
FORMAT SK3_3 N_2DIG.;
FORMAT SK3_4 N_2DIG.;
FORMAT SK3_5 N_2DIG.;
FORMAT SK4 $TSINO.;
FORMAT SK5 N_2DIG.;
FORMAT SK6 $T2EXPSC.;
FORMAT SK7 $T2EXPSC.;
FORMAT SK8 $T2EXPSC.;
FORMAT AL1 $T_2FREC.;
FORMAT AL2a_CER $TSINO.;
FORMAT AL2a_V1 N_2DIG.;
FORMAT AL2a_S1 N_2DIG.;
FORMAT AL2a_D1 N_2DIG.;
FORMAT AL2a_VIN $TSINO.;
FORMAT AL2a_V2 N_2DIG.;
FORMAT AL2a_S2 N_2DIG.;
FORMAT AL2a_D2 N_2DIG.;
FORMAT AL2a_LIC $TSINO.;
FORMAT AL2a_V3 N_2DIG.;
FORMAT AL2a_V4 N_2DIG.;
FORMAT AL2a_S3 N_2DIG.;
FORMAT AL2a_S4 N_2DIG.;
FORMAT AL2a_D3 N_2DIG.;
FORMAT AL2a_D4 N_2DIG.;
FORMAT AL2a_OTR $TSINO.;
FORMAT AL2a_V5 N_2DIG.;
FORMAT AL2a_S5 N_2DIG.;
FORMAT AL2a_D5 N_2DIG.;
FORMAT AL2a_LOC $TSINO.;
FORMAT AL2a_V6 N_2DIG.;
FORMAT AL2a_S6 N_2DIG.;
FORMAT AL2a_D6 N_2DIG.;
FORMAT AL2b_CER $TSINO.;
FORMAT AL2b_L1 N_2DIG.;
FORMAT AL2b_M1 N_2DIG.;
FORMAT AL2b_X1 N_2DIG.;
FORMAT AL2b_J1 N_2DIG.;
FORMAT AL2b_VIN $TSINO.;
FORMAT AL2b_L2 N_2DIG.;
FORMAT AL2b_M2 N_2DIG.;
FORMAT AL2b_X2 N_2DIG.;
FORMAT AL2b_J2 N_2DIG.;
FORMAT AL2b_LIC $TSINO.;
FORMAT AL2b_L3 N_2DIG.;
FORMAT AL2b_L4 N_2DIG.;
FORMAT AL2b_M3 N_2DIG.;
FORMAT AL2b_M4 N_2DIG.;
FORMAT AL2b_X3 N_2DIG.;
FORMAT AL2b_X4 N_2DIG.;
FORMAT AL2b_J3 N_2DIG.;
FORMAT AL2b_J4 N_2DIG.;
FORMAT AL2b_OTR $TSINO.;
FORMAT AL2b_L5 N_2DIG.;
FORMAT AL2b_M5 N_2DIG.;
FORMAT AL2b_X5 N_2DIG.;
FORMAT AL2b_J5 N_2DIG.;
FORMAT AL2b_LOC $TSINO.;
FORMAT AL2b_L6 N_2DIG.;
FORMAT AL2b_M6 N_2DIG.;
FORMAT AL2b_X6 N_2DIG.;
FORMAT AL2b_J6 N_2DIG.;
FORMAT AL3 $T_3FREC.;
FORMAT CN1 $TSINO.;
FORMAT CN2 $TSINO.;
FORMAT CN3 $TSINO.;
FORMAT CN4 $TSINO.;
FORMAT IN1_01 $T16SINO.;
FORMAT IN1_02 $T16SINO.;
FORMAT IN1_03 $T16SINO.;
FORMAT IN1_04 $T16SINO.;
FORMAT IN1_05 $T16SINO.;
FORMAT IN1_06 $T16SINO.;
FORMAT IN1_07 $T16SINO.;
FORMAT IN1_08 $T16SINO.;
FORMAT IN1_09 $T16SINO.;
FORMAT IN1_98 $T16SINO.;
FORMAT IN1_99 $T16SINO.;
FORMAT IN2 N_5DIG.;
FORMAT IN3 $T_INTVL.;

FORMAT IMC $T_IMC.;

RUN;
/* FIN PROGRAMA: Microdatos en SAS: EESEadulto_2009.sas*/
