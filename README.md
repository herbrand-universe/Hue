Hue
===

ToDo:
---
- [x] Hacer andar huetop en GHC 7.10
- [ ] Agregar al prompt de huetop la cardinalidad del contxto 'huetop[3]#'
- [ ] Estandarizar errores con una regexp
- [ ] Agregar commando undo, que hace pop de la últimia modificacion de estado
- [ ] Hacer andar el proofgeneral

Comandos:
---
- :load [< archivo >]
- :type < Term >
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

Hue proof-assistant

- [x] Hacer andar huetop en GHC 7.10
- [ ] Hacer andar el proofgeneral


Tablas
---

First Header | Second Header
------------ | -------------
Content from cell 1 | Content from cell 2
Content in the first column | Content in the second column
