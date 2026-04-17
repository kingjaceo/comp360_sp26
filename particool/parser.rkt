#lang brag
simulation : [system-defn] [system-defn]* [sim-defn] simulate

; system
system-defn:    /SYSTEM ID /LBRACE (system-feature)* /RBRACE
@system-feature: particles | center | velocity
particles: /PARTICLES /COLON INTEGER
center: /CENTER /COLON point
velocity: /VELOCITY /COLON point

; simulation
sim-defn:            /SIMULATION /LBRACE (simulation-feature)* /RBRACE
@simulation-feature: size | time
size: /SIZE /COLON point
time: /TIME /COLON number
simulate: /SIMULATE

; data structures
@point: /LPAREN number /COMMA number /RPAREN
@number: INTEGER | DECIMAL