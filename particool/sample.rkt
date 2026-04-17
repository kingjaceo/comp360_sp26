#lang particool

system Earth {
              particles:1000
              center:(0,0)
              velocity:(0,0)
              }

system Tundura {
                particles:100
                center:(-1,-1)
                velocity:(1,1)
               }

simulation {
            size:(100,100)
            time:100
           }

simulate