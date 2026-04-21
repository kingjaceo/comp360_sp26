#lang brag
simulation : [system-defn] [system-defn]* [sim-defn] simulate

; system
system-defn:    /SYSTEM ID /LBRACE (system-feature)* /RBRACE
@system-feature: particles | center | velocity | mass
particles: /PARTICLES /COLON INTEGER
center: /CENTER /COLON vector
velocity: /VELOCITY /COLON vector
mass: /MASS /COLON number

; simulation
sim-defn:            /SIMULATION /LBRACE (simulation-feature)* /RBRACE
@simulation-feature: size | time
size: /SIZE /COLON vector
time: /TIME /COLON number
simulate: /SIMULATE

; data structures
vector: /LPAREN number /COMMA number /RPAREN
@number: INTEGER | DECIMAL