/**********************************************************************************************************************					
Instituto Nacional de EstadÃƒÂ­stica (INE) www.ine.es					
***********************************************************************************************************************					
					
DESCRIPCIÃƒâ€œN:					
Este programa genera un fichero SAS con formatos, partiendo de un fichero sin ellos.					
					
Consta de las siguientes partes:					
	* 1. Definir la librerÃƒÂ­a de trabajo --> Libname				
	* 2. DefiniciÃƒÂ³n de formatos --> PROC FORMAT				
	* 3. Vincular formatos a la base de datos --> PASO data				
					
 Entrada:                                                           					
     - Fichero SAS sin formatos: 	 ESdEadulto_2023.sas7bdat				
 Salida:                                                           					
     - Fichero SAS con formatos: 	  ESdEadulto_2023_conFormato.sas7bdat				
					
Donde:					
	* OperaciÃƒÂ³n: Encuesta de Salud de España
	* Periodo: 2023
					
************************************************************************************************************************/					
		
/* Directorio de trabajo para la operaciÃƒÂ³n estadÃƒÂ­stica */
*%let siglas_periodo = ESdEadulto_2023;
*%let conFormato = _conFormato;
					
/*1) Definir la librerÃƒÂ­a de trabajo: introducir el directorio que desee como librerÃƒÂ­Ã‚Â­a					
(se da como ejemplo 'C:\Mis resultados'), y copiar en ese directorio el fichero sas "EPSH_2022.sas7bdat"*/					
					
*libname ROutput 'C:\Mis resultados';	

options fmtsearch = (ROutput ROutput.cat1);

* 2) DEFINICIÃƒâ€œN DE FORMATOS;
PROC FORMAT LIBRARY=ROutput.cat1;

*Tablas 1;
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
value $TSEXO

"1"="Hombre"
"2"="Mujer"
;
value $TSINOC

"1"="Sí"
"2"="No"
"9"="No contesta"
;
value $T1PROXY

"1"="La persona seleccionada está ingresada en un sanatorio, hospital, etc., a causa de una enfermedad"
"2"="La persona seleccionada está incapacitada para contestar por discapacidad, enfermedad grave, etc."
"3"="La persona seleccionada está incapacitada para contestar por causa del idioma"
;
value $T6PROXY

"1"="Cónyuge o pareja"
"2"="Hijo/a"
"3"="Padre/Madre"
"4"="Hermano/Hermana"
"5"="Otros familiares"
"6"="Servicios sociales"
"7"="Voluntarios"
"8"="Otra relación"
;
value $T1_A

"1"="Nacidos en España"
"2"="Nacidos en el extranjero"
"9"="No contesta"
;
value $TSINO

"1"="Sí"
"2"="No"
;
value $T4A

"1"="Conviviendo con su cónyuge"
"2"="Conviviendo con una pareja de hecho"
"3"="No conviviendo en pareja"
"9"="No contesta"
;
value $T5A

"1"="Soltero/a"
"2"="Casado/a"
"3"="Viudo/a"
"4"="Separado/a legalmente"
"5"="Divorciado/a"
"9"="No contesta"
;
value $TESTUD

"01"="No procede, es menor de 10 años"
"02"="No sabe leer o escribir"
"03"="Educación Primaria incompleta (Ha asistido menos de 5 años a la escuela)"
"04"="Educación Primaria completa"
"05"="Primera etapa de Enseñanza Secundaria, con o sin título (2º ESO aprobado, EGB, Bachillerato Elemental)"
"06"="Estudios de Bachillerato"
"07"="Enseñanzas profesionales de grado medio o equivalentes"
"08"="Enseñanzas profesionales de grado superior o equivalentes"
"09"="Estudios universitarios o equivalentes"
"99"="No contesta"
;
value $T1B

"1"="Sí, por cotización propia"
"2"="Sí, por cotización de otra persona (pensiones de viudedad, orfandad, etc.)"
"3"="Sí, por ambos tipos de cotización"
"4"="No"
"9"="No contesta"
;
value $T5B

"1"="Asalariado/a (a sueldo, comisión, jornal...)"
"2"="Empresario/a o profesional con asalariados"
"3"="Empresario/a sin asalariados o trabajador/a independiente"
"4"="Ayuda familiar (sin remuneración reglamentada en la empresa o negocio de un familiar)"
"5"="Miembro de una cooperativa"
"6"="Otra situación"
"9"="No contesta"
;
value $T6B

"1"="No ha trabajado nunca"
"2"="Menos de 6 meses"
"3"="De 6 meses a menos de 1 año"
"4"="De 1 año a menos de 2 años"
"5"="Más de 2 años"
"9"="No contesta"
;
value $T8B

"01"="Funcionario/a"
"02"="Duración indefinida"
"03"="Temporal"
"04"="Verbal o sin contrato"
"05"="Empresario/a o profesional con asalariados"
"06"="Empresario/a sin asalariados o trabajador/a independiente"
"07"="Ayuda familiar (sin remuneración reglamentada en la empresa o negocio de un familiar)"
"08"="Miembro de una cooperativa"
"09"="Otra situación"
"99"="No contesta"
;
value $T9aB

"1"="Menor de 6 meses"
"2"="De 6 meses a menos de 1 año"
"3"="De 1 año a menos de 2 años"
"4"="De 2 años o más"
"5"="Sin duración definida"
"9"="No contesta"
;
value $T9bB

"1"="Menor de 6 meses"
"2"="De 6 meses a menos de 1 año"
"3"="De 1 año a menos de 2 años"
"4"="De 2 años o más"
"5"="Sin duración definida"
"9"="No contesta"
;
value $T10B

"1"="No"
"2"="Sí, de 1 a 4 personas"
"3"="Sí, de 5 a 10 personas"
"4"="Sí, de 11 a 20 personas"
"5"="Sí, más de 20 personas"
"9"="No contesta"
;
value $T11B

"1"="A tiempo completo"
"2"="A tiempo parcial"
"9"="No contesta"
;
value $T12B

"01"="Jornada partida"
"02"="Jornada continua por la mañana"
"03"="Jornada continua por la tarde"
"04"="Jornada continua por la noche"
"05"="Jornada reducida"
"06"="Turnos"
"07"="Jornada irregular o variable según los días"
"08"="Otro tipo"
"99"="No contesta"
;
value $T15B

"1"="Asalariado/a (a sueldo, comisión, jornal...)"
"2"="Empresario/a o profesional con asalariados"
"3"="Empresario/a sin asalariados o trabajador/a independiente"
"4"="Ayuda familiar (sin remuneración reglamentada en la empresa o negocio de un familiar)"
"5"="Miembro de una cooperativa"
"6"="Otra situación"
"9"="No contesta"
;
value $T1C

"1"="Muy bueno"
"2"="Bueno"
"3"="Regular"
"4"="Malo"
"5"="Muy malo"
;
value $T3aC

"1"="Gravemente limitado/a"
"2"="Limitado/a, pero no gravemente"
"3"="Nada limitado/a"
"9"="No contesta"
;
value $T3bC

"1"="Físico"
"2"="Mental"
"3"="Ambos"
"9"="No contesta"
;
value $T4C

"1"="Muy bueno"
"2"="Bueno"
"3"="Regular"
"4"="Malo"
"5"="Muy malo"
"9"="No contesta"
;
value $T2D

"1"="Ingresó en un hospital"
"2"="Acudió a un centro de urgencias"
"3"="Consultó a un médico/a o enfermero/a"
"4"="No hizo ninguna consulta ni intervención"
"9"="No contesta"
;
value $T1F

"1"="Sí"
"2"="No"
"3"="Soy ciego o no puedo ver en absoluto"
"9"="No contesta"
;
value $T2F

"1"="No, ninguna dificultad"
"2"="Sí, alguna dificultad"
"3"="Sí, mucha dificultad"
"4"="No puedo ver en absoluto"
"9"="No contesta"
;
value $T3F

"1"="Sí"
"2"="No"
"3"="Soy sordo profundo"
"9"="No contesta"
;
value $T1DIFIC

"1"="No, ninguna dificultad"
"2"="Sí, alguna dificultad"
"3"="Sí, mucha dificultad"
"4"="No puedo hacerlo en absoluto"
"5"="No aplicable (nunca lo ha intentado o necesitado hacerlo)"
"9"="No contesta"
;
;
value $T2DIFIC

"1"="No, ninguna dificultad"
"2"="Sí, alguna dificultad"
"3"="Sí, mucha dificultad"
"4"="No puedo hacerlo por mí mismo"
"9"="No contesta"
;
value $T1AYUD

"1"="Sí, al menos para una actividad"
"2"="No"
"9"="No contesta"
;
value $T7G

"1"="Ninguno"
"2"="Muy leve"
"3"="Leve"
"4"="Moderado"
"5"="Severo"
"6"="Extremo"
"9"="No contesta"
;
value $T8G

"1"="Nada"
"2"="Un poco"
"3"="Moderadamente"
"4"="Bastante"
"5"="Mucho"
"9"="No contesta"
;
value $T1I

"1"="En las últimas 4 semanas"
"2"="Entre 4 semanas y 12 meses"
"3"="Hace 12 meses o más"
"4"="Nunca"
;
value $T3I

"1"="En las últimas 4 semanas"
"2"="Entre 4 semanas y 12 meses"
"3"="Hace 12 meses o más"
"4"="Nunca"
"9"="No contesta"
;
value $T5I

"1"="Médico de familia o médico general"
"2"="Especialista"
"9"="No contesta"
;
value $T6I

"01"="Centro de Salud / Consultorio"
"02"="Ambulatorio / Centro de especialidades"
"03"="Consulta externa de un hospital"
"04"="Servicio de urgencias no hospitalario"
"05"="Servicio de urgencias de un hospital"
"06"="Consulta de médico de una sociedad"
"07"="Consulta de médico particular"
"08"="Empresa o lugar de trabajo"
"09"="Domicilio del entrevistado"
"10"="Consulta telefónica"
"11"="Otro lugar"
"99"="No contesta"
;
value $T7I

"1"="Diagnóstico de una enfermedad o problema de salud"
"2"="Un accidente o agresión"
"3"="Revisión"
"4"="Sólo dispensación de recetas"
"5"="Parte de baja, confirmación o alta"
"6"="Otros motivos"
"9"="No contesta"
;
value $T8I

"1"="Sanidad Pública (Seguridad Social)"
"2"="Sociedad médica"
"3"="Consulta privada"
"4"="Otros (médico de empresa, etc.)"
"9"="No contesta"
;
value $T14I

"1"="Hace 3 meses o menos"
"2"="Hace más de 3 meses y menos de 6"
"3"="Hace 6 meses o más pero menos de 12"
"4"="Hace 12 meses o más"
"5"="Nunca"
;
value $T16I

"1"="Sanidad Pública (Seguridad Social, Ayuntamiento, etc.)"
"2"="Sociedad médica"
"3"="Consulta privada"
"4"="Otros"
"9"="No contesta"
;
value $T5J

"1"="Intervención quirúrgica"
"2"="Estudio médico para diagnóstico"
"3"="Tratamiento médico sin intervención quirúrgica"
"4"="Parto (incluye cesárea)"
"5"="Otros motivos"
"9"="No contesta"
;
value $T6J

"1"="Sanidad Pública (Seguridad Social)"
"2"="Mutualidad obligatoria (MUFACE, ISFAS, etc.)"
"3"="Sociedad médica privada"
"4"="A su propio cargo o de su hogar"
"5"="A cargo de otras personas, organismos o instituciones"
"9"="No contesta"
;
value $T14J

"1"="Hospital de la Sanidad Pública (Seguridad Social)"
"2"="Centro o servicio de urgencias no hospitalario de la Sanidad Pública (Seguridad Social)"
"3"="Sanatorio, hospital o clínica privada"
"4"="Servicio privado de urgencias"
"5"="Casa de socorro o servicio de urgencias del ayuntamiento"
"6"="Otro tipo de servicio"
"9"="No contesta"
;
value $T5L

"1"="En los últimos 12 meses"
"2"="Hace 1 año o más pero menos de 2 años"
"3"="Hace 2 años o más pero menos de 3 años"
"4"="Hace 3 años o más pero menos de 5 años"
"5"="Hace 5 años o más"
"9"="No contesta"
;
value $T7L

"1"="En los últimos 12 meses"
"2"="Hace 1 año o más pero menos de 3 años"
"3"="Hace 3 años o más pero menos de 5 años"
"4"="Hace 5 años o más"
"9"="No contesta"
;
value $T13L

"1"="En los últimos 12 meses"
"2"="Hace 1 año o más pero menos de 5 años"
"3"="Hace 5 años o más pero menos de 10 años"
"4"="Hace 10 años o más"
"9"="No contesta"
;
value $T15L

"1"="En los últimos 12 meses"
"2"="Hace 1 año o más pero menos de 2 años"
"3"="Hace 2 años o más pero menos de 3 años"
"4"="Hace 3 años o más"
"9"="No contesta"
;
;
value $T1M

"1"="Sí"
"2"="No"
"3"="No he necesitado asistencia médica"
"9"="No contesta"
;
value $T3_1M

"1"="Sí"
"2"="No"
"3"="No lo he necesitado"
"9"="No contesta"
;
value $T1O

"1"="Sentado/a la mayor parte de la jornada"
"2"="De pie la mayor parte de la jornada sin efectuar grandes desplazamientos o esfuerzos"
"3"="Caminando, llevando algún peso, efectuando desplazamientos frecuentes"
"4"="Realizando tareas que requieren gran esfuerzo físico"
"5"="No aplicable"
"9"="No contesta"
;
value $T2O

"1"="No hago ejercicio. El tiempo libre lo ocupo de forma casi completamente sedentaria"
"2"="Hago alguna actividad física o deportiva ocasional"
"3"="Hago actividad física varias veces al mes"
"4"="Hago entrenamiento deportivo o físico varias veces a la semana"
"9"="No contesta"
;
value $T4O

"1"="De 10 a 29 minutos"
"2"="De 30 a 59 minutos"
"3"="Una hora o más pero menos de 2 horas"
"4"="Dos horas o más pero menos de 3 horas"
"5"="Tres horas o más"
"9"="No contesta"
;
value $T1_1P

"1"="Una o más veces al día"
"2"="De 4 a 6 veces a la semana"
"3"="Tres veces a la semana"
"4"="Una o dos veces a la semana"
"5"="Menos de una vez a la semana"
"6"="Nunca"
"9"="No contesta"
;
value $T1Q

"1"="Sí, fumo a diario"
"2"="Sí fumo, pero no a diario"
"3"="No fumo actualmente, pero he fumado antes"
"4"="No fumo ni he fumado nunca de manera habitual"
"9"="No contesta"
;
value $T6Q

"1"="Nunca o casi nunca"
"2"="Menos de una hora al día"
"3"="Entre 1 y 5 horas al día"
"4"="Más de 5 horas al día"
"9"="No contesta"
;
value $T6aQ

"1"="Menos de una hora al día"
"2"="Entre 1 y 5 horas al día"
"3"="Más de 5 horas al día"
"9"="No contesta"
;
value $T7Q

"1"="Sí"
"2"="No actualmente, pero sí lo he hecho con anterioridad"
"3"="Nunca"
"9"="No contesta"
;
value $T8Q

"1"="Diariamente"
"2"="Ocasionalmente"
"9"="No contesta"
;
value $T1R

"01"="A diario o casi a diario"
"02"="5-6 días por semana"
"03"="3-4 días por semana"
"04"="1-2 días por semana"
"05"="2-3 días en un mes"
"06"="Una vez al mes"
"07"="Menos de una vez al mes"
"08"="No en los últimos 12 meses, he dejado de tomar alcohol"
"09"="Nunca o solamente unos sorbos para probarlo a lo largo de toda la vida"
"99"="No contesta"
;
value $T3R

"01"="A diario o casi a diario"
"02"="De 5 a 6 días por semana"
"03"="De 3 a 4 días por semana"
"04"="De 1 a 2 días por semana"
"05"="De 2 a 3 días en un mes"
"06"="Una vez al mes"
"07"="Menos de una vez al mes"
"08"="No en los últimos 12 meses"
"09"="Nunca en toda mi vida"
"99"="No contesta"
;
value $T1S

"1"="Ninguna"
"2"="Una o dos personas"
"3"="De 3 a 5 personas"
"4"="Más de 5 personas"
"9"="No contesta"
;
value $T2S

"1"="Mucho"
"2"="Algo"
"3"="Ni mucho ni poco"
"4"="Poco"
"5"="Nada"
"9"="No contesta"
;
value $T3S

"1"="Muy fácil"
"2"="Fácil"
"3"="Es posible"
"4"="Difícil"
"5"="Muy difícil"
"9"="No contesta"
;
value $T2T

"1"="Familiares"
"2"="Otras personas"
"9"="No contesta"
;
value $T3T

"1"="Menos de 10 horas a la semana"
"2"="10 horas o más a la semana pero menos de 20"
"3"="20 horas o más a la semana"
"9"="No contesta"
;
value $T_CLASE

"1"="Directores/as y gerentes de establecimientos de 10 o más asalariados/as y profesionales tradicionalmente asociados/as a las licenciaturas universitarias"
"2"="Directores/as y gerentes de establecimientos de menos de 10 asalariados/ as y profesionales tradicionalmente asociados/as a diplomaturas universitarias y otros/as profesionales de apoyo técnico. Deportistas y artistas"
"3"="Ocupaciones intermedias y trabajadores/as por cuenta propia"
"4"="Supervisores/as y trabajadores/as en ocupaciones técnicas cualificadas"
"5"="Trabajadores/as cualificados/as del sector primario y otros/as trabajadores/as semi-cualificados/as"
"6"="Trabajadores/as no cualificados/as"
"9"="No contesta"
;
value $T_IMC

"1"="Peso insuficiente"
"2"="Normopeso"
"3"="Sobrepeso"
"4"="Obesidad"
"9"="No consta"
;
value $TSEVERI

"1"="Ninguna"
"2"="Leve"
"3"="Moderada"
"4"="Moderadamente Grave"
"5"="Grave"
"9"="No consta"
;
value $TPREVAL

"1"="Cuadro depresivo mayor"
"2"="Otros cuadros depresivos"
"3"="Sin cuadro depresivo alguno"
"9"="No consta"
;

*Tablas 2;

value $T4CNO

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


*Tablas 3;

value $T3CNAE

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

*Tablas 4;
value N_999NC

999="No contesta"
;
value N_99NC

99="No contesta"
;
value N_9NC

9="No contesta"
;
value T3H

1="Nada estresante"
7="Muy estresante"
9="No contesta"
;
value T4H

1="Nada satisfactorio"
7="Muy satisfactorio"
9="No contesta"
;




* 3) VINCULAR FORMATOS A LA BASE DE DATOS;

	DATA ROutput.&siglas_periodo.&conFormato;
		set ROutput.&siglas_periodo;

FORMAT CCAA $TCCAA.;


FORMAT SEXOa $TSEXO.;
FORMAT EDADa N_999NC.;
FORMAT PROXY_0 $TSINO.;
FORMAT PROXY_1 $T1PROXY.;
FORMAT PROXY_2 $TSINO.;

FORMAT PROXY_4 $TSEXO.;
FORMAT PROXY_5 N_999NC.;
FORMAT PROXY_6 $T6PROXY.;
FORMAT A1a $T1_A.;
FORMAT A1c $T1_A.;
FORMAT A1e $T1_A.;
FORMAT A2a_1 $TSINO.;
FORMAT A2a_2 $TSINO.;
FORMAT A2a_3 $TSINO.;
FORMAT A3 N_99NC.;
FORMAT A4 $T4A.;
FORMAT A5 $T5A.;
FORMAT NIVEST $TESTUD.;
FORMAT B1 $T1B.;
FORMAT B2 $TSINOC.;
FORMAT B3_2 $T3CNAE.;
FORMAT B4_2 $T4CNO.;
FORMAT B5 $T5B.;
FORMAT B6 $T6B.;
FORMAT B7 $TSINOC.;
FORMAT B8 $T8B.;
FORMAT B9a $T9aB.;
FORMAT B9b $T9bB.;
FORMAT B10 $T10B.;
FORMAT B11 $T11B.;
FORMAT B12 $T12B.;
FORMAT B13a_2 $T3CNAE.;
FORMAT B13b_2 $T3CNAE.;
FORMAT B14a_2 $T4CNO.;
FORMAT B14b_2 $T4CNO.;
FORMAT B15 $T15B.;
FORMAT C1 $T1C.;
FORMAT C2 $TSINOC.;
FORMAT C3a $T3aC.;
FORMAT C3b $T3bC.;
FORMAT C4 $T4C.;
FORMAT C5a_1 $TSINOC.;
FORMAT C5b_1 $TSINOC.;
FORMAT C5c_1 $TSINOC.;
FORMAT C5a_2 $TSINOC.;
FORMAT C5b_2 $TSINOC.;
FORMAT C5c_2 $TSINOC.;
FORMAT C5d_2 $TSINOC.;
FORMAT C5a_3 $TSINOC.;
FORMAT C5b_3 $TSINOC.;
FORMAT C5c_3 $TSINOC.;
FORMAT C5a_4 $TSINOC.;
FORMAT C5b_4 $TSINOC.;
FORMAT C5c_4 $TSINOC.;
FORMAT C5a_5 $TSINOC.;
FORMAT C5b_5 $TSINOC.;
FORMAT C5c_5 $TSINOC.;
FORMAT C5a_6 $TSINOC.;
FORMAT C5b_6 $TSINOC.;
FORMAT C5c_6 $TSINOC.;
FORMAT C5a_7 $TSINOC.;
FORMAT C5b_7 $TSINOC.;
FORMAT C5c_7 $TSINOC.;
FORMAT C5a_8 $TSINOC.;
FORMAT C5b_8 $TSINOC.;
FORMAT C5c_8 $TSINOC.;
FORMAT C5a_9 $TSINOC.;
FORMAT C5b_9 $TSINOC.;
FORMAT C5c_9 $TSINOC.;
FORMAT C5a_10 $TSINOC.;
FORMAT C5b_10 $TSINOC.;
FORMAT C5c_10 $TSINOC.;
FORMAT C5a_11 $TSINOC.;
FORMAT C5b_11 $TSINOC.;
FORMAT C5c_11 $TSINOC.;
FORMAT C5a_12 $TSINOC.;
FORMAT C5b_12 $TSINOC.;
FORMAT C5c_12 $TSINOC.;
FORMAT C5a_13 $TSINOC.;
FORMAT C5b_13 $TSINOC.;
FORMAT C5c_13 $TSINOC.;
FORMAT C5a_14 $TSINOC.;
FORMAT C5b_14 $TSINOC.;
FORMAT C5c_14 $TSINOC.;
FORMAT C5a_15 $TSINOC.;
FORMAT C5b_15 $TSINOC.;
FORMAT C5c_15 $TSINOC.;
FORMAT C5a_16 $TSINOC.;
FORMAT C5b_16 $TSINOC.;
FORMAT C5c_16 $TSINOC.;
FORMAT C5a_17 $TSINOC.;
FORMAT C5b_17 $TSINOC.;
FORMAT C5c_17 $TSINOC.;
FORMAT C5a_18 $TSINOC.;
FORMAT C5b_18 $TSINOC.;
FORMAT C5c_18 $TSINOC.;
FORMAT C5a_19 $TSINOC.;
FORMAT C5b_19 $TSINOC.;
FORMAT C5c_19 $TSINOC.;
FORMAT C5a_20 $TSINOC.;
FORMAT C5b_20 $TSINOC.;
FORMAT C5c_20 $TSINOC.;
FORMAT C5a_21 $TSINOC.;
FORMAT C5b_21 $TSINOC.;
FORMAT C5c_21 $TSINOC.;
FORMAT C5a_22 $TSINOC.;
FORMAT C5b_22 $TSINOC.;
FORMAT C5c_22 $TSINOC.;
FORMAT C5a_23 $TSINOC.;
FORMAT C5b_23 $TSINOC.;
FORMAT C5c_23 $TSINOC.;
FORMAT C5a_24 $TSINOC.;
FORMAT C5b_24 $TSINOC.;
FORMAT C5c_24 $TSINOC.;
FORMAT C5d_24 $TSINOC.;
FORMAT C5a_25 $TSINOC.;
FORMAT C5b_25 $TSINOC.;
FORMAT C5c_25 $TSINOC.;
FORMAT C5a_26 $TSINOC.;
FORMAT C5b_26 $TSINOC.;
FORMAT C5c_26 $TSINOC.;
FORMAT C5a_27 $TSINOC.;
FORMAT C5b_27 $TSINOC.;
FORMAT C5c_27 $TSINOC.;
FORMAT C5a_28 $TSINOC.;
FORMAT C5b_28 $TSINOC.;
FORMAT C5c_28 $TSINOC.;
FORMAT C5a_29 $TSINOC.;
FORMAT C5b_29 $TSINOC.;
FORMAT C5c_29 $TSINOC.;
FORMAT C5a_30 $TSINOC.;
FORMAT C5b_30 $TSINOC.;
FORMAT C5c_30 $TSINOC.;
FORMAT C5a_31 $TSINOC.;
FORMAT C5b_31 $TSINOC.;
FORMAT C5c_31 $TSINOC.;
FORMAT C5a_32 $TSINOC.;
FORMAT C5b_32 $TSINOC.;
FORMAT C5c_32 $TSINOC.;
FORMAT C5a_33 $TSINOC.;
FORMAT C5b_33 $TSINOC.;
FORMAT C5c_33 $TSINOC.;
FORMAT D1_1 $TSINOC.;
FORMAT D1_2 $TSINOC.;
FORMAT D1_3 $TSINOC.;
FORMAT D2 $T2D.;
FORMAT E1 $TSINOC.;
FORMAT E2 N_999NC.;
FORMAT F1 $T1F.;
FORMAT F2 $T2F.;
FORMAT F3 $T3F.;
FORMAT F4 $T1DIFIC.;
FORMAT F5 $T1DIFIC.;
FORMAT F6 $T1DIFIC.;
FORMAT F7 $T1DIFIC.;
FORMAT F8 $T1DIFIC.;
FORMAT F9 $T1DIFIC.;
FORMAT G1_1 $T2DIFIC.;
FORMAT G1_2 $T2DIFIC.;
FORMAT G1_3 $T2DIFIC.;
FORMAT G1_4 $T2DIFIC.;
FORMAT G1_5 $T2DIFIC.;
FORMAT G2 $T1AYUD.;
FORMAT G3 $T1AYUD.;
FORMAT G4_1 $T1DIFIC.;
FORMAT G4_2 $T1DIFIC.;
FORMAT G4_3 $T1DIFIC.;
FORMAT G4_4 $T1DIFIC.;
FORMAT G4_5 $T1DIFIC.;
FORMAT G4_6 $T1DIFIC.;
FORMAT G4_7 $T1DIFIC.;
FORMAT G5 $T1AYUD.;
FORMAT G6 $T1AYUD.;
FORMAT G7 $T7G.;
FORMAT G8 $T8G.;
FORMAT H3 T3H.;
FORMAT H4 T4H.;
FORMAT I1 $T1I.;
FORMAT I2 N_99NC.;
FORMAT I3 $T3I.;
FORMAT I4 N_99NC.;
FORMAT I5 $T5I.;
FORMAT I6 $T6I.;
FORMAT I7 $T7I.;
FORMAT I8 $T8I.;
FORMAT I9_1 $TSINOC.;
FORMAT I9_2 $TSINOC.;
FORMAT I9_3 $TSINOC.;
FORMAT I10 $TSINOC.;
FORMAT I11_1 $TSINOC.;
FORMAT I11_2 $TSINOC.;
FORMAT I11_3 $TSINOC.;
FORMAT I11_4 $TSINOC.;
FORMAT I12_1 $TSINOC.;
FORMAT I12_2 $TSINOC.;
FORMAT I12_3 $TSINOC.;
FORMAT I12_4 $TSINOC.;
FORMAT I13_1 $TSINOC.;
FORMAT I13_2 $TSINOC.;
FORMAT I13_3 $TSINOC.;
FORMAT I13_4 $TSINOC.;
FORMAT I13_5 $TSINOC.;
FORMAT I14 $T14I.;
FORMAT I14b N_99NC.;
FORMAT I15_1 $TSINOC.;
FORMAT I15_2 $TSINOC.;
FORMAT I15_3 $TSINOC.;
FORMAT I15_4 $TSINOC.;
FORMAT I15_5 $TSINOC.;
FORMAT I15_6 $TSINOC.;
FORMAT I15_7 $TSINOC.;
FORMAT I15_8 $TSINOC.;
FORMAT I15_9 $TSINOC.;
FORMAT I15_10 $TSINOC.;
FORMAT I16 $T16I.;
FORMAT I17_1 $TSINOC.;
FORMAT I17_2 $TSINOC.;
FORMAT I17_3 $TSINOC.;
FORMAT I17_4 $TSINOC.;
FORMAT I17_5 $TSINOC.;
FORMAT I17_6 $TSINOC.;
FORMAT I17_7 $TSINOC.;
FORMAT I17_8 $TSINOC.;
FORMAT J1 $TSINO.;
FORMAT J2 N_99NC.;
FORMAT J3 N_999NC.;
FORMAT J4 $TSINOC.;
FORMAT J5 $T5J.;
FORMAT J6 $T6J.;
FORMAT J7 $TSINOC.;
FORMAT J8 N_999NC.;
FORMAT J9 $TSINO.;
FORMAT J10 N_999NC.;
FORMAT J11_1 $TSINOC.;
FORMAT J11_2 $TSINOC.;
FORMAT J11_3 $TSINOC.;
FORMAT J12_1 N_99NC.;
FORMAT J12_2 N_99NC.;
FORMAT J12_3 N_99NC.;
FORMAT J13_1 N_99NC.;
FORMAT J13_2 N_99NC.;
FORMAT J14 $T14J.;
FORMAT J15_1 $TSINOC.;
FORMAT J15_2 $TSINOC.;
FORMAT J15_3 $TSINOC.;
FORMAT J15_4 $TSINOC.;
FORMAT J15_5 $TSINOC.;
FORMAT J15_6 $TSINOC.;
FORMAT J15_7 $TSINOC.;
FORMAT K1 $TSINO.;
FORMAT K2 $TSINOC.;
FORMAT K3_1a $TSINOC.;
FORMAT K3_1b $TSINOC.;
FORMAT K3_2a $TSINOC.;
FORMAT K3_2b $TSINOC.;
FORMAT K3_3a $TSINOC.;
FORMAT K3_3b $TSINOC.;
FORMAT K3_4a $TSINOC.;
FORMAT K3_4b $TSINOC.;
FORMAT K3_5a $TSINOC.;
FORMAT K3_5b $TSINOC.;
FORMAT K3_6a $TSINOC.;
FORMAT K3_6b $TSINOC.;
FORMAT K3_7a $TSINOC.;
FORMAT K3_7b $TSINOC.;
FORMAT K3_8a $TSINOC.;
FORMAT K3_8b $TSINOC.;
FORMAT K3_9a $TSINOC.;
FORMAT K3_9b $TSINOC.;
FORMAT K3_10a $TSINOC.;
FORMAT K3_10b $TSINOC.;
FORMAT K3_11a $TSINOC.;
FORMAT K3_11b $TSINOC.;
FORMAT K3_12a $TSINOC.;
FORMAT K3_12b $TSINOC.;
FORMAT K3_13a $TSINOC.;
FORMAT K3_13b $TSINOC.;
FORMAT K3_14a $TSINOC.;
FORMAT K3_14b $TSINOC.;
FORMAT K3_15a $TSINOC.;
FORMAT K3_15b $TSINOC.;
FORMAT K3_16a $TSINOC.;
FORMAT K3_16b $TSINOC.;
FORMAT K3_17a $TSINOC.;
FORMAT K3_17b $TSINOC.;
FORMAT K3_18a $TSINOC.;
FORMAT K3_18b $TSINOC.;
FORMAT K3_19a $TSINOC.;
FORMAT K3_19b $TSINOC.;
FORMAT K3_20a $TSINOC.;
FORMAT K3_20b $TSINOC.;
FORMAT K3_21a $TSINOC.;
FORMAT K3_21b $TSINOC.;
FORMAT K3_22a $TSINOC.;
FORMAT K3_22b $TSINOC.;
FORMAT K3_23a $TSINOC.;
FORMAT K3_23b $TSINOC.;
FORMAT L1 $TSINO.;
FORMAT L4 $TSINOC.;
FORMAT L5 $T5L.;
FORMAT L6 $TSINOC.;
FORMAT L7 $T7L.;
FORMAT L8 $TSINOC.;
FORMAT L9 $T7L.;
FORMAT L10 $TSINOC.;
FORMAT L11 $T5L.;
FORMAT L12 $TSINOC.;
FORMAT L13 $T13L.;
FORMAT L14 $TSINOC.;
FORMAT L15 $T15L.;
FORMAT L16 $TSINOC.;
FORMAT L17 $T5L.;
FORMAT L18 $TSINOC.;
FORMAT L19 $T5L.;
FORMAT M1 $T1M.;
FORMAT M2 $T1M.;
FORMAT M3_1 $T3_1M.;
FORMAT M3_2 $T3_1M.;
FORMAT M3_3 $T3_1M.;
FORMAT M3_4 $T3_1M.;
FORMAT N1 N_999NC.;
FORMAT N2 N_999NC.;
FORMAT O1 $T1O.;
FORMAT O2 $T2O.;
FORMAT O3 N_9NC.;
FORMAT O4 $T4O.;
FORMAT O5 N_9NC.;
FORMAT O6 $T4O.;
FORMAT O7 N_9NC.;
FORMAT O8_1 N_99NC.;
FORMAT O8_2 N_99NC.;
FORMAT O9 N_9NC.;
FORMAT O10_1 N_99NC.;
FORMAT O10_2 N_99NC.;
FORMAT P1_1 $T1_1P.;
FORMAT P1_1a N_99NC.;
FORMAT P1_2 $T1_1P.;
FORMAT P1_3 $T1_1P.;
FORMAT P1_4 $T1_1P.;
FORMAT P1_5 $T1_1P.;
FORMAT P1_6 $T1_1P.;
FORMAT P1_7 $T1_1P.;
FORMAT P1_7a N_99NC.;
FORMAT P1_8 $T1_1P.;
FORMAT P1_9 $T1_1P.;
FORMAT P1_10 $T1_1P.;
FORMAT P1_11 $T1_1P.;
FORMAT P1_12 $T1_1P.;
FORMAT P1_13 $T1_1P.;
FORMAT P1_14 $T1_1P.;
FORMAT P1_15 $T1_1P.;
FORMAT P1_15a N_99NC.;
FORMAT P2 $TSINOC.;
FORMAT P3 N_99NC.;
FORMAT Q1 $T1Q.;
FORMAT Q2 N_99NC.;
FORMAT Q3 N_99NC.;
FORMAT Q4 $TSINOC.;
FORMAT Q5 N_99NC.;
FORMAT Q6 $T6Q.;
FORMAT Q6a $T6aQ.;
FORMAT Q7 $T7Q.;
FORMAT Q8 $T8Q.;
FORMAT R1 $T1R.;
FORMAT R2cer $TSINOC.;
FORMAT R2cer_1 N_99NC.;
FORMAT R2cer_2 N_99NC.;
FORMAT R2cer_3 N_99NC.;
FORMAT R2cer_4 N_99NC.;
FORMAT R2cer_5 N_99NC.;
FORMAT R2cer_6 N_99NC.;
FORMAT R2cer_7 N_99NC.;
FORMAT R2vin $TSINOC.;
FORMAT R2vin_1 N_99NC.;
FORMAT R2vin_2 N_99NC.;
FORMAT R2vin_3 N_99NC.;
FORMAT R2vin_4 N_99NC.;
FORMAT R2vin_5 N_99NC.;
FORMAT R2vin_6 N_99NC.;
FORMAT R2vin_7 N_99NC.;
FORMAT R2vermut $TSINOC.;
FORMAT R2vermut_1 N_99NC.;
FORMAT R2vermut_2 N_99NC.;
FORMAT R2vermut_3 N_99NC.;
FORMAT R2vermut_4 N_99NC.;
FORMAT R2vermut_5 N_99NC.;
FORMAT R2vermut_6 N_99NC.;
FORMAT R2vermut_7 N_99NC.;
FORMAT R2lic $TSINOC.;
FORMAT R2lic_1 N_99NC.;
FORMAT R2lic_2 N_99NC.;
FORMAT R2lic_3 N_99NC.;
FORMAT R2lic_4 N_99NC.;
FORMAT R2lic_5 N_99NC.;
FORMAT R2lic_6 N_99NC.;
FORMAT R2lic_7 N_99NC.;
FORMAT R2comb $TSINOC.;
FORMAT R2comb_1 N_99NC.;
FORMAT R2comb_2 N_99NC.;
FORMAT R2comb_3 N_99NC.;
FORMAT R2comb_4 N_99NC.;
FORMAT R2comb_5 N_99NC.;
FORMAT R2comb_6 N_99NC.;
FORMAT R2comb_7 N_99NC.;
FORMAT R2sidra $TSINOC.;
FORMAT R2sidra_1 N_99NC.;
FORMAT R2sidra_2 N_99NC.;
FORMAT R2sidra_3 N_99NC.;
FORMAT R2sidra_4 N_99NC.;
FORMAT R2sidra_5 N_99NC.;
FORMAT R2sidra_6 N_99NC.;
FORMAT R2sidra_7 N_99NC.;
FORMAT R3 $T3R.;
FORMAT S1 $T1S.;
FORMAT S2 $T2S.;
FORMAT S3 $T3S.;
FORMAT T1 $TSINOC.;
FORMAT T2 $T2T.;
FORMAT T3 $T3T.;

FORMAT CLASE_PR $T_CLASE.;
FORMAT IMC $T_IMC.;
FORMAT CMD1 N_999NC.;
FORMAT CMD2 N_999NC.;
FORMAT CMD3 N_999NC.;
FORMAT INDICE_BIENESTAR N_999NC.;
FORMAT SEVERIDAD_DEPRESIVA $TSEVERI.;
FORMAT CUADROS_DEPRESIVOS $TPREVAL.;


RUN;
/* FIN PROGRAMA: Microdatos en SAS: ESdEadulto_2023.sas*/
