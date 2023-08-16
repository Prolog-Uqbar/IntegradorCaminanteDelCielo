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
caracterizacion(yoda,ser(desconocido,5)).
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

%precede(EpisodioAnterior,EpisodioSiguiente)
precedeA(laAmenazaFantasma,elAtaqueDeLosClones).
precedeA(elAtaqueDeLosClones,laVenganzaDeLosSith).
precedeA(laVenganzaDeLosSith,unaNuevaEsperanza).
precedeA(unaNuevaEsperanza,elImperioContrataca).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
nuevoEpisodio(Heroe, Villano, Extra, Dispositivo) :-
    personajesDistintos(Heroe, Villano, Extra),
    heroe(Heroe),
    villano(Villano),
    extra(Extra,Heroe, Villano),
    reconocible(Dispositivo).

%%%%%%%%%%
personajesDistintos(Heroe, Villano, Extra) :-
    apareceEn(Heroe, _, _),
    apareceEn(Villano, _, _),
    apareceEn(Extra, _, _),
    Heroe \= Villano,
    Villano \= Extra,
    Heroe \= Extra.


%%%%%%%%%
heroe(Personaje) :-
    maestro(Personaje),
    apareceEn(Personaje, _, luminoso),
    not(apareceEn(Personaje, _, oscuro)).

%%%%%%%%%%%%%%%
villano(Villano) :-
    apareceEnMasDeUnEpisodio(Villano),
    cambiaDeLado(Villano).

apareceEnMasDeUnEpisodio(Personaje) :-
    apareceEn(Personaje, Episodio, _),
    apareceEn(Personaje, OtroEpisodio, _),
    Episodio \= OtroEpisodio.
    
cambiaDeLado(Personaje) :-
    apareceEn(Personaje, Episodio, luminoso),
    apareceEn(Personaje, Episodio, oscuro).

cambiaDeLado(Personaje) :-
    apareceEn(Personaje, EpisodioAnterior, luminoso),
    apareceEn(Personaje, EpisodioPosterior, oscuro),
    esAnterior(EpisodioAnterior, EpisodioPosterior).

esAnterior(Anterior, Siguiente) :- 
    precedeA(Anterior, Siguiente).
esAnterior(Anterior, Siguiente) :-
    precedeA(Anterior, Intermedio),
    esAnterior(Intermedio, Siguiente).

extra(Extra,Heroe,Villano):-
    aspectoExotico(Extra),
    forall(apareceEn(Extra, Episodio, _), apareceAlguno(Heroe, Villano, Episodio)).

aspectoExotico(Personaje):-
    caracterizacion(Personaje,Aspecto),
    exotico(Aspecto).

exotico(robot(Forma)) :- Forma \= esfera.
exotico(ser(_, Tamanio)) :- Tamanio > 15.
exotico(ser(desconocido, _)).

apareceAlguno(Protagonista, _, Episodio):-
    apareceEn(Protagonista, Episodio, _).
apareceAlguno(_, Protagonista, Episodio):-
    apareceEn(Protagonista, Episodio, _).

apareceDispositivo(Episodio, Dispositivo) :-
    elementosPresentes(Episodio, Elementos), 
    member(Dispositivo, Elementos).

reconocible(Dispositivo) :-
    apareceDispositivo(_, Dispositivo),
    findall(Episodio, apareceDispositivo(Episodio, Dispositivo), Episodios),
    length(Episodios, Cantidad),
    Cantidad >= 3.

% 1
% nuevoEpisodio(luke, vader, c3po, estrellaMuerte).
% true

% 2
% Las conformaciones posibles con la base de datos inicial son 
% nuevoEpisodio(luke, vader, c3po, Dispositivo).
% Dispositivo = sableLaser) ;
% Dispositivo = estrellaMuerte ;

% 3 Nuevos personajes

caracterizacion(bobaFett, clon(jangoFett)).
caracterizacion(darthSidious, sith(demacrado)).
caracterizacion(messi, nacionalidad(argentina)).
caracterizacion(harry, mago(100)).
caracterizacion(ron, mago(120)).
caracterizacion(hanSolo , equipamiento(halconMilenario , blaster)).
caracterizacion(pjRandom,omnipotente(1000)).
caracterizacion(jarjar, bizarro(gracioso)).
caracterizacion(tutu, bizarro(aburrido)).
caracterizacion(dinDjarin,cazarrecompenzas(armas)).
caracterizacion(bumbalu,alienigena(marte,100)). 
caracterizacion(kukuta,alienigena(saturno,20)).
caracterizacion(tumbalu,alienigena(tierra,60)).

exotico(clon(jangoFett)).
exotico(sith(_)).
exotico(nacionalidad(Pais)):-campeonDelMundo(Pais).
exotico(mago(Facha)):-
    caracterizacion(_, mago(OtraFacha)),
    Facha > OtraFacha.
exotico(equipamiento(_ , blaster)).
exotico(bicho(Anios)):- Anios > 3.
exotico(omnipotente(Edad)):- Edad >= 1000.
exotico(bizarro(gracioso)).
exotico(cazarrecompenzas(armas)).
exotico(alienigena(Planeta,Rareza)):-
    Planeta \= tierra,
    Rareza > 50.

campeonDelMundo(argentina).

% aspectoExotico(P).
% P = yoda ;
% P = jabba ;
% P = c3po ;
% P = r2d2 ;
% P = bobaFett ;
% P = darthSidious ;
% P = messi ;
% P = ron ;
% P = hanSolo ;
% P = pjRandom ;
% P = jarjar ;
% P = dinDjarin ;
% P = bumbalu ;
% false.