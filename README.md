Hue
===

ToDo:
---
- [x] Hacer andar huetop en GHC 7.10
- [x] Agregar al prompt de huetop la cardinalidad del contexto 'huetop[3]#', es decir, tiene 3 definiciones
- [ ] Estandarizar errores con una regexp
- [x] Agregar comando undo, que hace pop de la última modificacion de estado
- [x] Agregar comando undo_n, que hace n-pops sobre el contexto
- [ ] Agregar comando por borrar una prueba.
- [ ] Agregar comando para reiniciar hue, vuelve al estado inicial.
- [ ] Definir familias de comandos para PG, asi lo aislamos. Onda 'PG Restart', 'PG Undo' , 'PG Cd', 'PG Path', etc
- [ ] Hacer andar el proofgeneral

Comandos:
---
- :load [< archivo >]
- :type < Term >
- :undo < Integer >
- :conv < Term > = < Term >
- :quit

Lenguaje:
---
- import ;
- def < name > is < Type > = < Term >;
- var < name > is < Type >;
- proof < name > is < Term >;
- qed;

Terminos (Tipos)
---

```haskell

data CoCT = 
  |  asds
  |  sad
 * __\lambda__
 * __(M N)__
 * __(M N)__
```

Tacticas:
---
- intro ;
- assumption ;
- exact < Term >;
- apply ;
- left ;
- right ;
- elim ;
- unfold ;

Uso stack:
---
```bash
stack build
```
para compilar el proyecto.

```bash
stack exec Hue
```
para ejecutar Hue en el directorio actual.


Tablas
---

First Header | Second Header
------------ | -------------
Content from cell 1 | Content from cell 2
Content in the first column | Content in the second column
