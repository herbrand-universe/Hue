Hue
===

ToDo:
---
- [x] Hacer andar huetop en GHC 7.10
- [x] Agregar al prompt de huetop la cardinalidad del contexto 'huetop[3]#', es decir, tiene 3 definiciones
- [ ] Estandarizar errores con una regexp
- [x] Agregar comando undo, que hace pop de la última modificacion de estado
- [x] Agregar comando undo_n, que hace n-pops sobre el contexto
- [x] Agregar comando por borrar una prueba (kill).
- [ ] Agregar comando para reiniciar hue, vuelve al estado inicial.
- [ ] Definir familias de comandos para PG, asi lo aislamos. Onda 'PG Restart', 'PG Undo' , 'PG Cd', 'PG Path', etc
- [ ] Hacer andar el proofgeneral

Comandos:
---
- :load [< archivo >]
- :type < Term >
- pragma_undo < Integer >
- :conv < Term > = < Term >
- :quit

Lenguaje:
---
- import ;
- define < name > = < Term > : < Type >
- assume < name > : < Type >;
- proof < name > : < Term >
- qed

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
- intro 
- assumption 
- exact < Term >
- apply 
- left 
- right 
- elim 
- unfold 

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


Instalar Proof General
---

```bash
git clone https://github.com/ProofGeneral/PG ~/.emacs.d/lisp/PG
cd ~/.emacs.d/lisp/PG
```

Agregar la siguiente linea en el archivo ~/.emacs

```emacs
(load "~/.emacs.d/lisp/PG/generic/proof-site")
```

Instalar el modulo de Hue en Proof General
---
Agregar la siguiente linea en < proof-general-home > /generic/proof-site.el
adentro de la definición de `proof-assistant-table-default':

```emacs
	        (hue "Hue" "hue")
```

Luego copiar el directorio :

```bash
cp -R <repo>/proofgeneral/hue/ <proof-general-home>/hue/
```

Después se tiene que establecer el lugar donde buscar el binario de 'huetop'. Se debe editar el '~/.emacs'


```emacs
(setq hue-prog-name "<PATH>/huetop")

(load "~/.emacs.d/lisp/PG/generic/proof-site")

```
