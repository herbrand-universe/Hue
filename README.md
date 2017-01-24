Hue
===

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



Hue proof-assistant

- [x] Hacer andar huetop en GHC 7.10
- [ ] Hacer andar el proofgeneral
