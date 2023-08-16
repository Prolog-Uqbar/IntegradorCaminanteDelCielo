# Caminante Del Cielo

Ejercicio integrador 

**Un análisis predictivo de la saga más influyente del cine.**

_"El miedo lleva a la ira.
La ira lleva al odio.
El odio lleva al sufrimiento.
Y el sufrimiento al Lado Oscuro.
Cuidado con el miedo, joven padawan."_
_Maestro Yoda_

_"¡Es lógico!
pasaAlLadoOscuro(Alguien) :- tieneMiedo(Alguien)."
Joven Padawan de prolog_

**El objetivo principal es deducir las principales características del próximo episodio.**

En particular, se busca definir un predicado 

```
nuevoEpisodio(Heroe, Villano, Extra, Dispositivo). 
```

que permita relacionar a un personaje que sea el héroe del episodio con su correspondiente villano, junto con un personaje extra que le aporta mística y un dispositivo especial que resulta importante para la trama.

Las condiciones que deben cumplirse simultáneamente son las siguientes:
- No se quiere innovar tanto, los personajes deben haber aparecido en alguno de los episodios anteriores y obviamente ser diferentes.
- Para mantener el espíritu clásico, el héroe tiene que ser un jedi (un maestro que estuvo alguna vez en el lado luminoso) que nunca se haya pasado al lado oscuro. 
- El villano debe haber estado en más de un episodio y tiene que mantener algún rasgo de ambigüedad, por lo que se debe garantizar que haya aparecido del lado luminoso en algún episodio y del lado oscuro en el mismo episodio o en un episodio posterior.  
- El extra tiene que ser un personaje de aspecto exótico para mantener la estética de la saga. Tiene que tener un vínculo estrecho con los protagonistas, que consiste en que haya estado junto al heroe o al villano en todos los episodios en los que dicho extra apareció. Se considera exótico a los robots que no tengan forma de esfera y a los seres de gran tamaño (mayor a 15) o de especie desconocida.
- El dispositivo tiene que ser reconocible por el público, por lo que tiene que ser un elemento que haya estado presente en muchos episodios (3 o más)

1. Verificar si una determinada conformación del episodio es válida. 
Por ejemplo:

```
nuevoEpisodio(luke, vader, c3po, estrellaMuerte).
true
```

2. Encontrar todas las conformaciones posibles que se puedan armar. Mostrar ejemplos de consultas y respuestas

3. Agregar algunos personajes nuevos, con una manera propia de describir su aspecto y con una logica diferente para ser incluido como extras.

La información con la que se cuenta describe las principales características de los episodios.

```
%apareceEn( Personaje, Episodio, Lado de la luz).
apareceEn( luke, elImperioContrataca, luminoso).
apareceEn( luke, unaNuevaEsperanza, luminoso).
apareceEn( vader, unaNuevaEsperanza, oscuro).
apareceEn( vader, laVenganzaDeLosSith, luminoso).
apareceEn( vader, laAmenazaFantasma, luminoso).
apareceEn( c3po, laAmenazaFantasma, luminoso).
apareceEn( c3po, unaNuevaEsperanza, luminoso).
apareceEn( c3po, elImperioContrataca, luminoso).
apareceEn( chewbacca, elImperioContrataca, luminoso).
apareceEn( yoda, elAtaqueDeLosClones, luminoso).
apareceEn( yoda, laAmenazaFantasma, luminoso).

%Maestro(Personaje)
maestro(luke).
maestro(leia).
maestro(vader).
maestro(yoda).
maestro(rey).
maestro(duku).

%caracterizacion(Personaje,Aspecto).
%aspectos:
% ser(Especie,Tamaño)
% humano
% robot(Forma)
caracterizacion(chewbacca,ser(wookiee,10)).
caracterizacion(luke,humano).
caracterizacion(vader,humano).
caracterizacion(yoda,ser(desconocido,5).
caracterizacion(jabba,ser(hutt,20)).
caracterizacion(c3po,robot(humanoide)).
caracterizacion(bb8,robot(esfera)).
caracterizacion(r2d2,robot(secarropas)).

%elementosPresentes(Episodio, Dispositivos)
elementosPresentes(laAmenazaFantasma, [sableLaser]).
elementosPresentes(elAtaqueDeLosClones, [sableLaser, clon]).
elementosPresentes(laVenganzaDeLosSith, [sableLaser, mascara, estrellaMuerte]).
elementosPresentes(unaNuevaEsperanza, [estrellaMuerte, sableLaser, halconMilenario]).
elementosPresentes(elImperioContrataca, [mapaEstelar, estrellaMuerte] ).

El orden de los episodios se representa de la siguiente manera:
%precede(EpisodioAnterior,EpisodioSiguiente)
precedeA(laAmenazaFantasma,elAtaqueDeLosClones).
precedeA(elAtaqueDeLosClones,laVenganzaDeLosSith).
precedeA(laVenganzaDeLosSith,unaNuevaEsperanza).
precedeA(unaNuevaEsperanza,elImperioContrataca).

```

**Importante:**
Se pueden agregar nuevos hechos para tener más ejemplos y alternativas. 
No se deben agregar nuevos predicados que sean hechos, pero sí definir predicados con reglas auxiliares.
