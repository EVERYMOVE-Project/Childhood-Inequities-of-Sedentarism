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
     - Fichero SAS sin formatos: 	 EESEhogar_2014.sas7bdat				
 Salida:                                                           					
     - Fichero SAS con formatos: 	 EESEhogar_2014_conFormato.sas7bdat				
					
Donde:					
	* Operación: EESEhogar Encuesta Europea de Salud en España. Cuestionario hogar				
	* Periodo: 2014				
					
************************************************************************************************************************/					
					
/*1) Definir la librería de trabajo: introducir el directorio que desee como librería					
(se da como ejemplo 'C:\Mis resultados'), y copiar en ese directorio el fichero sas "EESEhogar_2014.sas7bdat"*/					
					
	
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
value $TESTRAT

"0"="Municipios de más de 500.000 habitantes"
"1"="Municipio capital de provincia (excepto los anteriores)"
"2"="Municipios con más de 100.000 habitantes (excepto los anteriores)"
"3"="Municipios de 50.000 a 100.000 habitantes (excepto los anteriores)"
"4"="Municipios de 20.000 a 50.000 habitantes (excepto los anteriores)"
"5"="Municipios de 10.000 a 20.000 habitantes"
"6"="Municipios con menos de 10.000 habitantes"
;
value $TSEXO

"1"="Hombre"
"2"="Mujer"
;
value $TA8_2_i

"01"="Adulto seleccionado (a.s.)"
"02"="Cónyuge o pareja del adulto seleccionado"
"03"="Hijo/a o hijastro/a (del adulto seleccionado o pareja del mismo)"
"04"="Yerno, nuera (o pareja del hijo/a o hijastro/a)"
"05"="Nieto/a o nieto/a político o pareja del mismo"
"06"="Padre, madre, suegro, suegra (o pareja de los mismos)"
"07"="Hermano/a"
"08"="Otro pariente del adulto seleccionado (o de la pareja del mismo)"
"09"="Persona del servicio doméstico"
"10"="Sin parentesco con el adulto seleccionado"
;
value $TA9_otr

"1"="Otro familiar no miembro del hogar"
"2"="Un cuidador personal"
"3"="Un empleado/a del servicio doméstico"
"4"="Servicios Sociales"
"5"="Otros (un vecino, el portero, etc.)"
"8"="No sabe"
"9"="No contesta"
;
value $TA10_i

"01"="No procede, es menor de 10 años"
"02"="No sabe leer o escribir"
"03"="Educación Primaria incompleta (Ha asistido menos de 5 años a la escuela)"
"04"="Educación Primaria completa"
"05"="Primera etapa de Enseñanza Secundaria, con o sin título (2º ESO aprobado, EGB, Bachillerato Elemental)"
"06"="Estudios de Bachillerato"
"07"="Enseñanzas profesionales de grado medio o equivalentes"
"08"="Enseñanzas profesionales de grado superior o equivalentes"
"09"="Estudios universitarios o equivalentes"
"98"="No sabe"
"99"="No contesta"
;
value $TA11_i

"1"="Trabajando"
"2"="En desempleo"
"3"="Jubilado/a, prejubilado/a"
"4"="Estudiando"
"5"="Incapacitado/a para trabajar"
"6"="Las labores del hogar"
"7"="Otros"
"8"="No contesta"
;
value $T12A

"1"="Hogar unipersonal"
"2"="Pareja sola"
"3"="Pareja con algún hijo menor de 25 años"
"4"="Pareja con todos los hijos de 25 o más años"
"5"="Padre o madre solo, con algún hijo menor de 25 años"
"6"="Padre o madre solo, con todos los hijos de 25 o más años"
"7"="Pareja o padre o madre solo, con algún hijo menor de 25 años y otras personas viviendo en el hogar"
"8"="Otro tipo de hogar"
;
value $T13B

"1"="Sí, por cotización propia"
"2"="Sí, por cotización de otra persona (pensiones de viudedad, orfandad, etc.)"
"3"="Sí, por ambos tipos de cotización"
"4"="No"
"8"="No sabe"
"9"="No contesta"
;
value $T17B

"1"="Asalariado/a (a sueldo, comisión, jornal...)"
"2"="Empresario/a o profesional con asalariados"
"3"="Empresario/a sin asalariados o trabajador/a independiente"
"4"="Ayuda familiar (sin remuneración reglamentada en la empresa o negocio de un familiar)"
"5"="Miembro de una cooperativa"
"6"="Otra situación"
"8"="No sabe"
"9"="No contesta"
;
value $TB21a

"1"="Asalariado/a (a sueldo, comisión, jornal...)"
"2"="Empresario/a o profesional con asalariados"
"3"="Empresario/a sin asalariados o trabajador/a independiente"
"4"="Ayuda familiar (sin remuneración reglamentada en la empresa o negocio de un familiar)"
"5"="Miembro de una cooperativa"
"6"="Otra situación"
"8"="No sabe"
"9"="No contesta"
;
value $TB21b

"1"="Asalariado/a (a sueldo, comisión, jornal...)"
"2"="Empresario/a o profesional con asalariados"
"3"="Empresario/a sin asalariados o trabajador/a independiente"
"4"="Ayuda familiar (sin remuneración reglamentada en la empresa o negocio de un familiar)"
"5"="Miembro de una cooperativa"
"6"="Otra situación"
"8"="No sabe"
"9"="No contesta"
;
value $TCLASE

"1"="Directores/as y gerentes de establecimientos de 10 o más asalariados/as y profesionales tradicionalmente asociados/as a las licenciaturas universitarias"
"2"="Directores/as y gerentes de establecimientos de menos de 10 asalariados/ as y profesionales tradicionalmente asociados/as a diplomaturas universitarias y otros/as profesionales de apoyo técnico. Deportistas y artistas"
"3"="Ocupaciones intermedias y trabajadores/as por cuenta propia"
"4"="Supervisores/as y trabajadores/as en ocupaciones técnicas cualificadas"
"5"="Trabajadores/as cualificados/as del sector primario y otros/as trabajadores/as semi-cualificados/as"
"6"="Trabajadores/as no cualificados/as"
"8"="No sabe"
"9"="No contesta"
;
value $T24D

"1"="Ingresos del trabajo (por cuenta propia o ajena)"
"2"="Prestación y subsidios por desempleo"
"3"="Pensión por jubilación o viudedad"
"4"="Pensión por invalidez o incapacidad"
"5"="Prestaciones económicas por hijo/a a cargo, ayudas a la familia..."
"6"="Prestaciones o subvenciones relacionadas con la vivienda"
"7"="Prestaciones o subvenciones relacionadas con la educación"
"8"="Otros ingresos regulares / Otro subsidio o prestación social regular"
;
value $T26D

"01"="Menos de 970 euros"
"02"="De 970 a menos de 1400 euros"
"03"="De 1400 a menos de 2040 euros"
"04"="De 2040 a menos de 3280 euros"
"05"="De 3280 euros en adelante"
"98"="No sabe"
"99"="No contesta"
;
value $T2SINO

"1"="Sí"
"2"="No"
;
value $TSINOC

"1"="Sí"
"2"="No"
"8"="No sabe"
"9"="No contesta"
;
value $T11CNO

"001"="Oficiales y suboficiales de las fuerzas armadas"
"002"="Tropa y marinería de las fuerzas armadas"
"111"="Miembros del poder ejecutivo y de los cuerpos legislativos; directivos de la Administración Pública y organizaciones de interés social"
"112"="Directores generales y presidentes ejecutivos"
"121"="Directores de departamentos administrativos"
"122"="Directores comerciales, de publicidad, relaciones públicas y de investigación y desarrollo"
"131"="Directores de producción de explotaciones agropecuarias, forestales y pesqueras, y de industrias manufactureras, de minería, construcción y distribución"
"132"="Directores de servicios de tecnologías de la información y las comunicaciones (TIC) y de empresas de servicios profesionales"
"141"="Directores y gerentes de empresas de alojamiento"
"142"="Directores y gerentes de empresas de restauración"
"143"="Directores y gerentes de empresas de comercio al por mayor y al por menor"
"150"="Directores y gerentes de otras empresas de servicios no clasificados bajo otros epígrafes"
"211"="Médicos"
"212"="Profesionales de enfermería y partería"
"213"="Veterinarios"
"214"="Farmacéuticos"
"215"="Otros profesionales de la salud"
"221"="Profesores de universidades y otra enseñanza superior (excepto formación profesional)"
"222"="Profesores de formación profesional (materias específicas)"
"223"="Profesores de enseñanza secundaria (excepto materias específicas de formación profesional)"
"224"="Profesores de enseñanza primaria"
"225"="Maestros y educadores de enseñanza infantil"
"231"="Profesores y técnicos de educación especial"
"232"="Otros profesores y profesionales de la enseñanza"
"241"="Físicos, químicos, matemáticos y afines"
"242"="Profesionales en ciencias naturales"
"243"="Ingenieros (excepto ingenieros agrónomos, de montes, eléctricos, electrónicos y TIC)"
"244"="Ingenieros eléctricos, electrónicos y de telecomunicaciones"
"245"="Arquitectos, urbanistas e ingenieros geógrafos"
"246"="Ingenieros técnicos (excepto agrícolas, forestales, eléctricos, electrónicos y TIC)"
"247"="Ingenieros técnicos en electricidad, electrónica y telecomunicaciones"
"248"="Arquitectos técnicos, topógrafos y diseñadores"
"251"="Jueces, magistrados, abogados y fiscales"
"259"="Otros profesionales del derecho"
"261"="Especialistas en finanzas"
"262"="Especialistas en organización y administración"
"263"="Técnicos de empresas y actividades turísticas"
"264"="Profesionales de ventas técnicas y médicas (excepto las TIC)"
"265"="Otros profesionales de las ventas, la comercialización, la publicidad y las relaciones públicas"
"271"="Analistas y diseñadores de software y multimedia"
"272"="Especialistas en bases de datos y en redes informáticas"
"281"="Economistas"
"282"="Sociólogos, historiadores, psicólogos y otros profesionales en ciencias sociales"
"283"="Sacerdotes de las distintas religiones"
"291"="Archivistas, bibliotecarios, conservadores y afines"
"292"="Escritores, periodistas y lingüistas"
"293"="Artistas creativos e interpretativos"
"311"="Delineantes y dibujantes técnicos"
"312"="Técnicos de las ciencias físicas, químicas, medioambientales y de las ingenierías"
"313"="Técnicos en control de procesos"
"314"="Técnicos de las ciencias naturales y profesionales auxiliares afines"
"315"="Profesionales en navegación marítima y aeronáutica"
"316"="Técnicos de control de calidad de las ciencias físicas, químicas y de las ingenierías"
"320"="Supervisores en ingeniería de minas, de industrias manufactureras y de la construcción"
"331"="Técnicos sanitarios de laboratorio, pruebas diagnósticas y prótesis"
"332"="Otros técnicos sanitarios"
"333"="Profesionales de las terapias alternativas"
"340"="Profesionales de apoyo en finanzas y matemáticas"
"351"="Agentes y representantes comerciales"
"352"="Otros agentes comerciales"
"353"="Agentes inmobiliarios y otros agentes"
"361"="Asistentes administrativos y especializados"
"362"="Agentes de aduanas, tributos y afines que trabajan en tareas propias de la Administración Pública"
"363"="Técnicos de las fuerzas y cuerpos de seguridad"
"371"="Profesionales de apoyo de servicios jurídicos y sociales"
"372"="Deportistas, entrenadores, instructores de actividades deportivas; monitores de actividades recreativas"
"373"="Técnicos y profesionales de apoyo de actividades culturales, artísticas y culinarias"
"381"="Técnicos en operaciones de tecnologías de la información y asistencia al usuario"
"382"="Programadores informáticos"
"383"="Técnicos en grabación audiovisual, radiodifusión y telecomunicaciones"
"411"="Empleados contables y financieros"
"412"="Empleados de registro de materiales, de servicios de apoyo a la producción y al transporte"
"421"="Empleados de bibliotecas y archivos"
"422"="Empleados de servicios de correos, codificadores, correctores y servicios de personal"
"430"="Otros empleados administrativos sin tareas de atención al público"
"441"="Empleados de información y recepcionistas (excepto de hoteles)"
"442"="Empleados de agencias de viajes, recepcionistas de hoteles y telefonistas"
"443"="Agentes de encuestas"
"444"="Empleados de ventanilla y afines (excepto taquilleros)"
"450"="Empleados administrativos con tareas de atención al público no clasificados bajo otros epígrafes"
"500"="Camareros y cocineros propietarios"
"511"="Cocineros asalariados"
"512"="Camareros asalariados"
"521"="Jefes de sección de tiendas y almacenes"
"522"="Vendedores en tiendas y almacenes"
"530"="Comerciantes propietarios de tiendas"
"541"="Vendedores en quioscos o en mercadillos"
"542"="Operadores de telemarketing"
"543"="Expendedores de gasolineras"
"549"="Otros vendedores"
"550"="Cajeros y taquilleros (excepto bancos)"
"561"="Auxiliares de enfermería"
"562"="Técnicos auxiliares de farmacia y emergencias sanitarias y otros trabajadores de los cuidados a las personas en servicios de salud"
"571"="Trabajadores de los cuidados personales a domicilio (excepto cuidadores de niños)"
"572"="Cuidadores de niños"
"581"="Peluqueros y especialistas en tratamientos de estética, bienestar y afines"
"582"="Trabajadores que atienden a viajeros, guías turísticos y afines"
"583"="Supervisores de mantenimiento y limpieza de edificios, conserjes y mayordomos domésticos"
"584"="Trabajadores propietarios de pequeños alojamientos"
"589"="Otros trabajadores de servicios personales"
"591"="Guardias civiles"
"592"="Policías"
"593"="Bomberos"
"594"="Personal de seguridad privado"
"599"="Otros trabajadores de los servicios de protección y seguridad"
"611"="Trabajadores cualificados en actividades agrícolas (excepto en huertas, invernaderos, viveros y jardines)"
"612"="Trabajadores cualificados en huertas, invernaderos, viveros y jardines"
"620"="Trabajadores cualificados en actividades ganaderas (incluidas avícolas, apícolas y similares)"
"630"="Trabajadores cualificados en actividades agropecuarias mixtas"
"641"="Trabajadores cualificados en actividades forestales y del medio natural"
"642"="Trabajadores cualificados en actividades pesqueras y acuicultura"
"643"="Trabajadores cualificados en actividades cinegéticas"
"711"="Trabajadores en hormigón, encofradores, ferrallistas y afines"
"712"="Albañiles, canteros, tronzadores, labrantes y grabadores de piedras"
"713"="Carpinteros (excepto ebanistas y montadores de estructuras metálicas)"
"719"="Otros trabajadores de las obras estructurales de construcción"
"721"="Escayolistas y aplicadores de revestimientos de pasta y mortero"
"722"="Fontaneros e instaladores de tuberías"
"723"="Pintores, empapeladores y afines"
"724"="Soladores, colocadores de parquet y afines"
"725"="Mecánicos-instaladores de refrigeración y climatización"
"729"="Otros trabajadores de acabado en la construcción, instalaciones (excepto electricistas) y afines"
"731"="Moldeadores, soldadores, chapistas, montadores de estructuras metálicas y trabajadores afines"
"732"="Herreros y trabajadores de la fabricación de herramientas y afines"
"740"="Mecánicos y ajustadores de maquinaria"
"751"="Electricistas de la construcción y afines"
"752"="Otros instaladores y reparadores de equipos eléctricos"
"753"="Instaladores y reparadores de equipos electrónicos y de telecomunicaciones"
"761"="Mecánicos de precisión en metales, ceramistas, vidrieros y artesanos"
"762"="Oficiales y operarios de las artes gráficas"
"770"="Trabajadores de la industria de la alimentación, bebidas y tabaco"
"781"="Trabajadores que tratan la madera y afines"
"782"="Ebanistas y trabajadores afines"
"783"="Trabajadores del textil, confección, piel, cuero y calzado"
"789"="Pegadores, buceadores, probadores de productos y otros operarios y artesanos diversos"
"811"="Operadores en instalaciones de la extracción y explotación de minerales"
"812"="Operadores en instalaciones para el tratamiento de metales"
"813"="Operadores de instalaciones y máquinas de productos químicos, farmacéuticos y materiales fotosensibles"
"814"="Operadores en instalaciones para el tratamiento y transformación de la madera, la fabricación de papel, productos de papel y caucho o materias plásticas"
"815"="Operadores de máquinas para fabricar productos textiles y artículos de piel y de cuero"
"816"="Operadores de máquinas para elaborar productos alimenticios, bebidas y tabaco"
"817"="Operadores de máquinas de lavandería y tintorería"
"819"="Otros operadores de instalaciones y maquinaria fijas"
"820"="Montadores y ensambladores en fábricas"
"831"="Maquinistas de locomotoras y afines"
"832"="Operadores de maquinaria agrícola y forestal móvil"
"833"="Operadores de otras máquinas móviles"
"834"="Marineros de puente, marineros de máquinas y afines"
"841"="Conductores de automóviles, taxis y furgonetas"
"842"="Conductores de autobuses y tranvías"
"843"="Conductores de camiones"
"844"="Conductores de motocicletas y ciclomotores"
"910"="Empleados domésticos"
"921"="Personal de limpieza de oficinas, hoteles y otros establecimientos similares"
"922"="Limpiadores de vehículos, ventanas y personal de limpieza a mano"
"931"="Ayudantes de cocina"
"932"="Preparadores de comidas rápidas"
"941"="Vendedores callejeros"
"942"="Repartidores de publicidad, limpiabotas y otros trabajadores de oficios callejeros"
"943"="Ordenanzas, mozos de equipaje, repartidores a pie y afines"
"944"="Recogedores de residuos, clasificadores de desechos, barrenderos y afines"
"949"="Otras ocupaciones elementales"
"951"="Peones agrícolas"
"952"="Peones ganaderos"
"953"="Peones agropecuarios"
"954"="Peones de la pesca, la acuicultura, forestales y de la caza"
"960"="Peones de la construcción y de la minería"
"970"="Peones de las industrias manufactureras"
"981"="Peones del transporte, descargadores y afines"
"982"="Reponedores"
"000"="No contesta"
;
value $T09CNAE

"011"="Cultivos no perennes"
"012"="Cultivos perennes"
"013"="Propagación de plantas"
"014"="Producción ganadera"
"015"="Producción agrícola combinada con la producción ganadera"
"016"="Actividades de apoyo a la agricultura, a la ganadería y de preparación posterior a la cosecha"
"017"="Caza, captura de animales y servicios relacionados con las mismas"
"021"="Silvicultura y otras actividades forestales"
"022"="Explotación de la madera"
"023"="Recolección de productos silvestres, excepto madera"
"024"="Servicios de apoyo a la silvicultura"
"031"="Pesca"
"032"="Acuicultura"
"051"="Extracción de antracita y hulla"
"052"="Extracción de lignito"
"061"="Extracción de crudo de petróleo"
"062"="Extracción de gas natural"
"071"="Extracción de minerales de hierro"
"072"="Extracción de minerales metálicos no férreos"
"081"="Extracción de piedra, arena y arcilla"
"089"="Industrias extractivas n.c.o.p."
"091"="Actividades de apoyo a la extracción de petróleo y gas natural"
"099"="Actividades de apoyo a otras industrias extractivas"
"101"="Procesado y conservación de carne y elaboración de productos cárnicos"
"102"="Procesado y conservación de pescados, crustáceos y moluscos"
"103"="Procesado y conservación de frutas y hortalizas"
"104"="Fabricación de aceites y grasas vegetales y animales"
"105"="Fabricación de productos lácteos"
"106"="Fabricación de productos de molinería, almidones y productos amiláceos"
"107"="Fabricación de productos de panadería y pastas alimenticias"
"108"="Fabricación de otros productos alimenticios"
"109"="Fabricación de productos para la alimentación animal"
"110"="Fabricación de bebidas"
"120"="Industria del tabaco"
"131"="Preparación e hilado de fibras textiles"
"132"="Fabricación de tejidos textiles"
"133"="Acabado de textiles"
"139"="Fabricación de otros productos textiles"
"141"="Confección de prendas de vestir, excepto de peletería"
"142"="Fabricación de artículos de peletería"
"143"="Confección de prendas de vestir de punto"
"151"="Preparación, curtido y acabado del cuero; fabricación de artículos de marroquinería, viaje y de guarnicionería y talabartería; preparación y teñido de pieles"
"152"="Fabricación de calzado"
"161"="Aserrado y cepillado de la madera"
"162"="Fabricación de productos de madera, corcho, cestería y espartería"
"171"="Fabricación de pasta papelera, papel y cartón"
"172"="Fabricación de artículos de papel y de cartón"
"181"="Artes gráficas y servicios relacionados con las mismas"
"182"="Reproducción de soportes grabados"
"191"="Coquerías"
"192"="Refino de petróleo"
"201"="Fabricación de productos químicos básicos, compuestos nitrogenados, fertilizantes, plásticos y caucho sintético en formas primarias"
"202"="Fabricación de pesticidas y otros productos agroquímicos"
"203"="Fabricación de pinturas, barnices y revestimientos similares; tintas de imprenta y masillas"
"204"="Fabricación de jabones, detergentes y otros artículos de limpieza y abrillantamiento; fabricación de perfumes y cosméticos"
"205"="Fabricación de otros productos químicos"
"206"="Fabricación de fibras artificiales y sintéticas"
"211"="Fabricación de productos farmacéuticos de base"
"212"="Fabricación de especialidades farmacéuticas"
"221"="Fabricación de productos de caucho"
"222"="Fabricación de productos de plástico"
"231"="Fabricación de vidrio y productos de vidrio"
"232"="Fabricación de productos cerámicos refractarios"
"233"="Fabricación de productos cerámicos para la construcción"
"234"="Fabricación de otros productos cerámicos"
"235"="Fabricación de cemento, cal y yeso"
"236"="Fabricación de elementos de hormigón, cemento y yeso"
"237"="Corte, tallado y acabado de la piedra"
"239"="Fabricación de productos abrasivos y productos minerales no metálicos n.c.o.p."
"241"="Fabricación de productos básicos de hierro, acero y ferroaleaciones"
"242"="Fabricación de tubos, tuberías, perfiles huecos y sus accesorios, de acero"
"243"="Fabricación de otros productos de primera transformación del acero"
"244"="Producción de metales preciosos y de otros metales no férreos"
"245"="Fundición de metales"
"251"="Fabricación de elementos metálicos para la construcción"
"252"="Fabricación de cisternas, grandes depósitos y contenedores de metal"
"253"="Fabricación de generadores de vapor, excepto calderas de calefacción central"
"254"="Fabricación de armas y municiones"
"255"="Forja, estampación y embutición de metales; metalurgia de polvos"
"256"="Tratamiento y revestimiento de metales; ingeniería mecánica por cuenta de terceros"
"257"="Fabricación de artículos de cuchillería y cubertería, herramientas y ferretería"
"259"="Fabricación de otros productos metálicos"
"261"="Fabricación de componentes electrónicos y circuitos impresos ensamblados"
"262"="Fabricación de ordenadores y equipos periféricos"
"263"="Fabricación de equipos de telecomunicaciones"
"264"="Fabricación de productos electrónicos de consumo"
"265"="Fabricación de instrumentos y aparatos de medida, verificación y navegación; fabricación de relojes"
"266"="Fabricación de equipos de radiación, electromédicos y electroterapéuticos"
"267"="Fabricación de instrumentos de óptica y equipo fotográfico"
"268"="Fabricación de soportes magnéticos y ópticos"
"271"="Fabricación de motores, generadores y transformadores eléctricos, y de aparatos de distribución y control eléctrico"
"272"="Fabricación de pilas y acumuladores eléctricos"
"273"="Fabricación de cables y dispositivos de cableado"
"274"="Fabricación de lámparas y aparatos eléctricos de iluminación"
"275"="Fabricación de aparatos domésticos"
"279"="Fabricación de otro material y equipo eléctrico"
"281"="Fabricación de maquinaria de uso general"
"282"="Fabricación de otra maquinaria de uso general"
"283"="Fabricación de maquinaria agraria y forestal"
"284"="Fabricación de máquinas herramienta para trabajar el metal y otras máquinas herramienta"
"289"="Fabricación de otra maquinaria para usos específicos"
"291"="Fabricación de vehículos de motor"
"292"="Fabricación de carrocerías para vehículos de motor; fabricación de remolques y semirremolques"
"293"="Fabricación de componentes, piezas y accesorios para vehículos de motor"
"301"="Construcción naval"
"302"="Fabricación de locomotoras y material ferroviario"
"303"="Construcción aeronáutica y espacial y su maquinaria"
"304"="Fabricación de vehículos militares de combate"
"309"="Fabricación de otro material de transporte n.c.o.p."
"310"="Fabricación de muebles"
"321"="Fabricación de artículos de joyería, bisutería y similares"
"322"="Fabricación de instrumentos musicales"
"323"="Fabricación de artículos de deporte"
"324"="Fabricación de juegos y juguetes"
"325"="Fabricación de instrumentos y suministros médicos y odontológicos"
"329"="Industrias manufactureras n.c.o.p."
"331"="Reparación de productos metálicos, maquinaria y equipo"
"332"="Instalación de máquinas y equipos industriales"
"351"="Producción, transporte y distribución de energía eléctrica"
"352"="Producción de gas; distribución por tubería de combustibles gaseosos"
"353"="Suministro de vapor y aire acondicionado"
"360"="Captación, depuración y distribución de agua"
"370"="Recogida y tratamiento de aguas residuales"
"381"="Recogida de residuos"
"382"="Tratamiento y eliminación de residuos"
"383"="Valorización"
"390"="Actividades de descontaminación y otros servicios de gestión de residuos"
"411"="Promoción inmobiliaria"
"412"="Construcción de edificios"
"421"="Construcción de carreteras y vías férreas, puentes y túneles"
"422"="Construcción de redes"
"429"="Construcción de otros proyectos de ingeniería civil"
"431"="Demolición y preparación de terrenos"
"432"="Instalaciones eléctricas, de fontanería y otras instalaciones en obras de construcción"
"433"="Acabado de edificios"
"439"="Otras actividades de construcción especializada"
"451"="Venta de vehículos de motor"
"452"="Mantenimiento y reparación de vehículos de motor"
"453"="Comercio de repuestos y accesorios de vehículos de motor"
"454"="Venta, mantenimiento y reparación de motocicletas y de sus repuestos y accesorios"
"461"="Intermediarios del comercio"
"462"="Comercio al por mayor de materias primas agrarias y de animales vivos"
"463"="Comercio al por mayor de productos alimenticios, bebidas y tabaco"
"464"="Comercio al por mayor de artículos de uso doméstico"
"465"="Comercio al por mayor de equipos para las tecnologías de la información y las comunicaciones"
"466"="Comercio al por mayor de otra maquinaria, equipos y suministros"
"467"="Otro comercio al por mayor especializado"
"469"="Comercio al por mayor no especializado"
"471"="Comercio al por menor en establecimientos no especializados"
"472"="Comercio al por menor de productos alimenticios, bebidas y tabaco en establecimientos especializados"
"473"="Comercio al por menor de combustible para la automoción en establecimientos especializados"
"474"="Comercio al por menor de equipos para las tecnologías de la información y las comunicaciones en establecimientos especializados"
"475"="Comercio al por menor de otros artículos de uso doméstico en establecimientos especializados"
"476"="Comercio al por menor de artículos culturales y recreativos en establecimientos especializados"
"477"="Comercio al por menor de otros artículos en establecimientos especializados"
"478"="Comercio al por menor en puestos de venta y en mercadillos"
"479"="Comercio al por menor no realizado ni en establecimientos, ni en puestos de venta ni en mercadillos"
"491"="Transporte interurbano de pasajeros por ferrocarril"
"492"="Transporte de mercancías por ferrocarril"
"493"="Otro transporte terrestre de pasajeros"
"494"="Transporte de mercancías por carretera y servicios de mudanza"
"495"="Transporte por tubería"
"501"="Transporte marítimo de pasajeros"
"502"="Transporte marítimo de mercancías"
"503"="Transporte de pasajeros por vías navegables interiores"
"504"="Transporte de mercancías por vías navegables interiores"
"511"="Transporte aéreo de pasajeros"
"512"="Transporte aéreo de mercancías y transporte espacial"
"521"="Depósito y almacenamiento"
"522"="Actividades anexas al transporte"
"531"="Actividades postales sometidas a la obligación del servicio universal"
"532"="Otras actividades postales y de correos"
"551"="Hoteles y alojamientos similares"
"552"="Alojamientos turísticos y otros alojamientos de corta estancia"
"553"="Campings y aparcamientos para caravanas"
"559"="Otros alojamientos"
"561"="Restaurantes y puestos de comidas"
"562"="Provisión de comidas preparadas para eventos y otros servicios de comidas"
"563"="Establecimientos de bebidas"
"581"="Edición de libros, periódicos y otras actividades editoriales"
"582"="Edición de programas informáticos"
"591"="Actividades cinematográficas, de vídeo y de programas de televisión"
"592"="Actividades de grabación de sonido y edición musical"
"601"="Actividades de radiodifusión"
"602"="Actividades de programación y emisión de televisión"
"611"="Telecomunicaciones por cable"
"612"="Telecomunicaciones inalámbricas"
"613"="Telecomunicaciones por satélite"
"619"="Otras actividades de telecomunicaciones"
"620"="Programación, consultoría y otras actividades relacionadas con la informática"
"631"="Proceso de datos, hosting y actividades relacionadas; portales web"
"639"="Otros servicios de información"
"641"="Intermediación monetaria"
"642"="Actividades de las sociedades holding"
"643"="Inversión colectiva, fondos y entidades financieras similares"
"649"="Otros servicios financieros, excepto seguros y fondos de pensiones"
"651"="Seguros"
"652"="Reaseguros"
"653"="Fondos de pensiones"
"661"="Actividades auxiliares a los servicios financieros, excepto seguros y fondos de pensiones"
"662"="Actividades auxiliares a seguros y fondos de pensiones"
"663"="Actividades de gestión de fondos"
"681"="Compraventa de bienes inmobiliarios por cuenta propia"
"682"="Alquiler de bienes inmobiliarios por cuenta propia"
"683"="Actividades inmobiliarias por cuenta de terceros"
"691"="Actividades jurídicas"
"692"="Actividades de contabilidad, teneduría de libros, auditoría y asesoría fiscal"
"701"="Actividades de las sedes centrales"
"702"="Actividades de consultoría de gestión empresarial"
"711"="Servicios técnicos de arquitectura e ingeniería y otras actividades relacionadas con el asesoramiento técnico"
"712"="Ensayos y análisis técnicos"
"721"="Investigación y desarrollo experimental en ciencias naturales y técnicas"
"722"="Investigación y desarrollo experimental en ciencias sociales y humanidades"
"731"="Publicidad"
"732"="Estudio de mercado y realización de encuestas de opinión pública"
"741"="Actividades de diseño especializado"
"742"="Actividades de fotografía"
"743"="Actividades de traducción e interpretación"
"749"="Otras actividades profesionales, científicas y técnicas n.c.o.p."
"750"="Actividades veterinarias"
"771"="Alquiler de vehículos de motor"
"772"="Alquiler de efectos personales y artículos de uso doméstico"
"773"="Alquiler de otra maquinaria, equipos y bienes tangibles"
"774"="Arrendamiento de la propiedad intelectual y productos similares, excepto trabajos protegidos por los derechos de autor"
"781"="Actividades de las agencias de colocación"
"782"="Actividades de las empresas de trabajo temporal"
"783"="Otra provisión de recursos humanos"
"791"="Actividades de agencias de viajes y operadores turísticos"
"799"="Otros servicios de reservas y actividades relacionadas con los mismos"
"801"="Actividades de seguridad privada"
"802"="Servicios de sistemas de seguridad"
"803"="Actividades de investigación"
"811"="Servicios integrales a edificios e instalaciones"
"812"="Actividades de limpieza"
"813"="Actividades de jardinería"
"821"="Actividades administrativas y auxiliares de oficina"
"822"="Actividades de los centros de llamadas"
"823"="Organización de convenciones y ferias de muestras"
"829"="Actividades de apoyo a las empresas n.c.o.p."
"841"="Administración Pública y de la política económica y social"
"842"="Prestación de servicios a la comunidad en general"
"843"="Seguridad Social obligatoria"
"851"="Educación preprimaria"
"852"="Educación primaria"
"853"="Educación secundaria"
"854"="Educación postsecundaria"
"855"="Otra educación"
"856"="Actividades auxiliares a la educación"
"861"="Actividades hospitalarias"
"862"="Actividades médicas y odontológicas"
"869"="Otras actividades sanitarias"
"871"="Asistencia en establecimientos residenciales con cuidados sanitarios"
"872"="Asistencia en establecimientos residenciales para personas con discapacidad intelectual, enfermedad mental y drogodependencia"
"873"="Asistencia en establecimientos residenciales para personas mayores y con discapacidad física"
"879"="Otras actividades de asistencia en establecimientos residenciales"
"881"="Actividades de servicios sociales sin alojamiento para personas mayores y con discapacidad"
"889"="Otros actividades de servicios sociales sin alojamiento"
"900"="Actividades de creación, artísticas y espectáculos"
"910"="Actividades de bibliotecas, archivos, museos y otras actividades culturales"
"920"="Actividades de juegos de azar y apuestas"
"931"="Actividades deportivas"
"932"="Actividades recreativas y de entretenimiento"
"941"="Actividades de organizaciones empresariales, profesionales y patronales"
"942"="Actividades sindicales"
"949"="Otras actividades asociativas"
"951"="Reparación de ordenadores y equipos de comunicación"
"952"="Reparación de efectos personales y artículos de uso doméstico"
"960"="Otros servicios personales"
"970"="Actividades de los hogares como empleadores de personal doméstico"
"981"="Actividades de los hogares como productores de bienes para uso propio"
"982"="Actividades de los hogares como productores de servicios para uso propio"
"990"="Actividades de organizaciones y organismos extraterritoriales"
"000"="No contesta"
;
value N_3DIG

998="No sabe"
999="No contesta"
;
value N_2DIG

98="No sabe"
99="No contesta"
;


* 3) VINCULAR FORMATOS A LA BASE DE DATOS;
data ROutput.EESEhogar_2014_ConFormato;
	set ROutput.EESEhogar_2014;

FORMAT CCAA $TCCAA.;
FORMAT ESTRATO $TESTRAT.;
FORMAT SEXO_i $TSEXO.;
FORMAT EDAD_i N_3DIG.;
FORMAT NADULTOS N_2DIG.;
FORMAT NMENORES N_2DIG.;
FORMAT A7_1_i $T2SINO.;
FORMAT A8_1_i $T2SINO.;
FORMAT A8_2_i $TA8_2_i.;
FORMAT A9_otra $TA9_otr.;
FORMAT A10_i $TA10_i.;
FORMAT A11_i $TA11_i.;
FORMAT A12 $T12A.;
FORMAT B13 $T13B.;
FORMAT B14 $TSINOC.;
FORMAT B15_2 $T09CNAE.;
FORMAT B16_2 $T11CNO.;
FORMAT B17 $T17B.;
FORMAT B18 $TSINOC.;
FORMAT B19a_2 $T09CNAE.;
FORMAT B19b_2 $T09CNAE.;
FORMAT B20a_2 $T11CNO.;
FORMAT B20b_2 $T11CNO.;
FORMAT B21a $TB21a.;
FORMAT B21b $TB21b.;

FORMAT CLASE_PR $TCLASE.;
FORMAT D23_1 $T2SINO.;
FORMAT D23_2 $T2SINO.;
FORMAT D23_3 $T2SINO.;
FORMAT D23_4 $T2SINO.;
FORMAT D23_5 $T2SINO.;
FORMAT D23_6 $T2SINO.;
FORMAT D23_7 $T2SINO.;
FORMAT D23_8 $T2SINO.;
FORMAT D23_9 $T2SINO.;
FORMAT D23_10 $T2SINO.;
FORMAT D23_11 $T2SINO.;
FORMAT D24 $T24D.;
FORMAT D26 $T26D.;


RUN;
/* FIN PROGRAMA: Microdatos en SAS: EESEhogar_2014.sas*/
