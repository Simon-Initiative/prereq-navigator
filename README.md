# Pre-requisite based navigator

This is a WIP, prototype component that allows a student to navigate through
OLI course content based on a graphical display of the pre-requisite 
relationships defined between workbook pages. 

To build this:

First: `elm install`, then

```
elm make src/Main.elm --output=navigator.js --debug
```

To run it:

```
elm reactor --port 8002
```

Then visit `localhost:8002`

