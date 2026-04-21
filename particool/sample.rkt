#lang particool

system Earth {
              particles:1
              center:(150,150)
              velocity:(0,0)
              ; total mass?
              ; particle size? / system radius?
              }

system Moon {
                particles:1
                center:(150,100)
                velocity:(1,0)
               }

simulation {
            size:(700,700)
            time:100
           }

simulate