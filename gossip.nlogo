;; GOSSIP GROUP CODE
;; SFI CSSS 2010

;; GLOBAL TODO 1: have a table of how far from the truth you are.  
;; How to do this was discussed on 06/14/10 so that the agent doesn't actually know how close to the truth their value actually is

;; GLOBAL TODO 2: Add decision rules

;; GLOBAL TODO 3: Add fitness rules


;; GLOBAL VARIABLES
globals
[
  global-sent?        ;; is true if any message was sent in previous tick, false if no message was sent in previous tick (so therefore no more will be sent)
  gossip-setup-flag?   ;; set to true is gossip has been setup for this iteration
  send-threshold      ;; low bound for when to stop sending 
  ;;Megan: why is the threshold global and not a turtle parameter?
  maximum-fitness     ;; the maximum fitness amongst all agents (for normalizing node sizes)
  liardecrease
  endevent?
  outfilename
  event-number
  target ;; turtle ID of current target
]

;; OUR AGENTS
turtles-own
[
  ;; persistent properties
  liar?               ;; if true, the turtle is a liar
  fitness             ;; accumulated fitness (or perhaps "wealth")
  my-decision-rule    ;; which decision rule to use when choosing among messages to propagate
  target?             ;; if true, the turtle is the current gossip target

  ;; event-specific properties
  just-informed?      ;; if true, the turtle has been given the gossip in this turn
  has-belief?         ;; if true, the turtle has an opinion on the truth
  belief-value        ;; value of currently held belief (0..1)
  received-weight     ;; cumulative value for weight received from message currently believed
  sender-id           ;; id of turtle who sent the currently believed message
  old-received-weight ;; weight of message decided on previously (some decision rules need to know if it is new or not)
  old-belief-value    ;; purely for remembering previous belief value; this may end up needing to be a list
  old-sender-id       ;; remembering id of previously believed belief
  received-messages   ;; list of all messages and their weights, received the current time step
]

;; PROPERTY OF LINKS BETWEEN NODES
links-own
[
  weight              ;; links are weighted
]

;; Creates links between specified nodes with a random weight
;; Creates thickness for display of node based on weight
;; TODO: (distant in future) have the weight of the link based on personal likelihood of spreading gossip
to create-random-link-with [anode]
  create-link-with anode [
    set weight random-float 1.0
    set thickness weight * 0.5
  ]
end

to setup
  clear-all
  setup-nodes
  setup-spatially-clustered-network
  ask n-of initial-liar-count turtles
    [ become-liar ]
  update-plot
  set liardecrease 0.75
  set endevent? false
  set event-number 0
  set outfilename user-new-file
  file-open outfilename
end

;; set up all nodes, including initializing their agent values
to setup-nodes
  set-default-shape turtles "circle"
  set maximum-fitness 0
  crt number-of-nodes
  [
    ; for visual reasons, we don't put any nodes *too* close to the edges
    setxy (random-xcor * 0.95) (random-ycor * 0.95)
    initialize ; this function is only called to set things in setup
  ]
end

;; TODO: make a specific type of network
to setup-spatially-clustered-network
  let num-links (average-node-degree * number-of-nodes) / 2
  while [count links < num-links ]
  [
    ask one-of turtles
    [
      let choice (min-one-of (other turtles with [not link-neighbor? myself])
                   [distance myself])
      if choice != nobody [ create-random-link-with choice ]
    ]
  ]
  ; make the network look a little prettier
  repeat 10
  [
    layout-spring turtles links 0.3 (world-width / (sqrt number-of-nodes)) 1
  ]
end

;; set up some turtles to be observers of a particular gossip-worthy event
;; set up who is the target of the gossip
to start-gossip
  ask n-of initial-observer-count turtles
    [ become-informed 1.0 1.0 -1]
  ;need to define a target but make sure its not one of the observers
  choose-target
end

;need to define a target but make sure its not one of the observers
to choose-target
  ;choose who will be target, ensure they aren't an observer
  let rand -1
  while [rand < 0]
  [
    set rand random number-of-nodes - 1; because turtle IDs start at zero
  ]
  let belief? false
  ask turtle rand [ set belief? has-belief?]
  while [belief?] ; only way we know they are an observer, is they have a belief
    [
    set rand random number-of-nodes - 1
    if rand >= 0 [
      ask turtle rand [set belief? has-belief?]
  ]
    ]
  
  ;set target as target
  set target rand 
  ask turtle rand [
    set target? true
    set belief-value 1.0
    set has-belief? true
    set color [0 0 255]
  ]
end

; should be called before nodes are defined as observers
to initialize
  set liar? false
 ; set received-weight 0.0
  set send-threshold 0.5
  set fitness -1.0
  forget
  set gossip-setup-flag? false
  set-decision-rule
end

; should be called when setting up for a new event, and only then
to forget
  set received-weight 0.0
  set just-informed? false
  set has-belief? false
  set belief-value 1.0
  set sender-id -1
  set target? false
  set received-messages []
  set old-received-weight 0.0
  set old-belief-value 0
  set old-sender-id -1
  set color [255 255 255]
end

; Start or continue an endless gossip cycle (should be a forever button)
; runs the repeated event/gossip cycles until the user sets repeat-events? to off.
; represents the round of propagation of a single scandalous 'event' and all the gossip involved
; TODO: calculate fitness after each gossip cycle
to go
  let event-finished? iterate
end

; Start or continue one gossip cycle (should be a forever button)
to gossip
  let event-finished? iterate
  if event-finished? [stop]
end

; Step through one gossip cycle  (should not be a forever button)
to step
  let event-finished? iterate
end

; runs the repeated event/gossip cycles until the user sets repeat-events? to off.
; represents the round of propagation of a single scandalous 'event' and all the gossip involved
; lots of flags set and unset here because netlogo doesn't have sophisticated ways of pausing and resuming
to-report iterate
  set endevent? false
  ; Have we set up this round of gossip yet?
  if not gossip-setup-flag? [
    ; Set up a gossip event.
    ; forget any previous gossip
    set event-number event-number + 1
    saveFitness
    ask turtles
    [ 
      forget 
    ]
    
    ; generate a new gossip event
    start-gossip
    set global-sent? true         ; need to be true so that first time through loop will work
    set gossip-setup-flag? true   ; but we don't want to set up repeatedly
  ]
  ;now, loop until everyone who will know does know
  spread-gossip
  set endevent? global-sent?
  tick
  update-plot
    
  ifelse global-sent? [ ; if we received events this round we report that we are not ready to stop
    report false
  ]
  [ ;If nothing happened we are in equilibrium so the chitchat round is over. clean up
      update-fitnesses
      set gossip-setup-flag? false;
      
      if spread-truth [
        target-share-truth   
      ]
      
      report true         ; report event ended in case we wish to stop here
      set endevent? true
      show "end"
  ]
end

;target tells its neighbors what the truth is
;neighbors then adjust their connection to the person who told them the gossip
to target-share-truth
  ask turtle target [
     ask link-neighbors [
       receive-truth [belief-value] of myself
     ]
  ]
end

; called by target-share-truth, turtle this is called on is being given the truth
to receive-truth [true-belief]
  if sender-id >= 0 and has-belief? [ 
    ;get current weight
    let old-weight [weight] of link-with turtle sender-id
    let new-weight old-weight
    
    ;calculate new weight
    ifelse (true-belief - belief-value >= 0.2) [
      set new-weight old-weight * (1 - (true-belief - belief-value) / 2)
    ]
    [
      set new-weight old-weight + (1 - old-weight) / 4
    ]
    ;if new weight is 0, remove the link
    ;otherwise, set the link weight to the new link weight
    ifelse new-weight = 0 [
      ask link who sender-id [die]
    ]
    [
      ask link who sender-id [
        set weight new-weight
        set thickness weight * 0.5
        set color [0 0 255]
      ]
    ]
  ]

end

; used during initialization only
to become-liar
  set liar? true
  set shape "square"
end

; used when you receive a message
; only add this message and weight to your list of recieved messages and weights
to become-informed [ pass-value pass-weight pass-id];; turtle procedure
  if not target? [
    set just-informed? true 
    set has-belief? true
    let topass []
    set topass lput pass-weight topass
    ifelse pass-value < 0
    [ set topass lput 0 topass]
    [ set topass lput pass-value topass]
    set topass lput pass-id topass
    set received-messages lput topass received-messages
  ]
end

; all turtles with messages will send them to neighbors based on the weight and threshold
; TODO: need truth to be related to scale of truthiness
; TODO: have a better rule for received weighting than last-value-wins
to spread-gossip
  set global-sent? false ; initialize to false at beginning of each tick
  ask turtles with [just-informed?]
    [ 
      let id who
      decide ; choose which received message is the one to propagate
      ask link-neighbors
      [
        ; first find the weight of the link, then find that weight multiplied by the sender's receivedweight
        let outgoing-weight [weight] of link-with myself
        let this-weight (([received-weight] of myself) * outgoing-weight) ; Warning - this takes forever to decay for large link strneght
        
        ; if the weight shows that we should send the message based on using it as a probability
        ; and the weight is also above the threshold of when to no longer send,
        ; send to the neighbor (truth value based on state as liar) 
        ;let rand random-float 1.0
        if send-threshold < (this-weight) ;and rand < this-weight
        [
          set global-sent? true ; set to true so globally we know at least 1 message was propagated
          ; send your weighted message as either true or false based on liar
          ; TODO: this will change based on truth table
          ifelse ([liar?] of myself)
            [ become-informed ([belief-value] of myself * liardecrease) this-weight id]
            [ become-informed [belief-value] of myself this-weight id]
        ]
      ]
      set just-informed? false; since has sent message, don't want to resend next time
    ]
end

to update-plot
  set-current-plot "believers"
  set-current-plot-pen "has-belief"
  plot (count turtles with [has-belief?]) / (count turtles) * 100
  set-current-plot-pen "true-belief"
  plot (count turtles with [ belief-value > 0.5 and has-belief? ]) / (count turtles) * 100
  
  set-current-plot "fitness plot"
  set-current-plot-pen "fitness-pen"
  histogram [fitness] of turtles
  auto-plot-on
end

; calculate everyone's fitness and update
; TODO: have different payoffs for liars and truth-tellers
; TODO: base it on the mean fitness
to update-fitnesses
  ask turtles with [has-belief?] [
    ;set fitness (fitness + (truthiness belief-value))
    set fitness (truthiness belief-value)
    ;set size (fitness * 0.1 + 1.0)
    set size (fitness * 2.0 + 0.1)
  ]
end

to-report allfitness

let reportlist []
  if endevent? [
  ask turtles [
    if has-belief? and fitness > -1[
      set reportlist lput fitness reportlist
    ]
  ]
  ]
 ; print reportlist
  report reportlist
end

; assumes this is only called at the end of an event
to saveFitness
  let total 0
  let counting 0
  ask turtles [
    if has-belief? and fitness > -1[
      set counting counting + 1
      set total total + fitness
    ]
  ]
  if counting > 0
  [set total total / counting]
  ;let currfilename (word outfilename event-number)
  file-type event-number
  file-type " "
  file-print total
end

; report the distance of an individual belief from truth. Easy when it's binary!
to-report truthiness [value]
  report value
end

; set own decision rule. This is necessary for when we let turtles have different decision rules.
; does NetLogo have a better way to do this??
to set-decision-rule
  ifelse Decision-Rule = "Mode"[
    set my-decision-rule 1
  ]
  [ ifelse Decision-Rule = "Random"[
    set my-decision-rule 2
    ]
    [ ifelse Decision-Rule = "Average"[
      set my-decision-rule 3
      ]
      [  set my-decision-rule 4
      ]
     ]
    ]
end

; used each time step.  There must be a better way to do this.
to decide

  ;remember most recent history
  if received-weight > 0[
    set old-received-weight received-weight
    set old-belief-value belief-value
    set old-sender-id sender-id
  ]
  ;use appropriate decision function
  ifelse my-decision-rule = 1 [
    decide-mode
  ]
  [ ifelse my-decision-rule = 2[
      decide-random
    ]
    [ ifelse my-decision-rule = 3[
        decide-average
      ]
      [ decide-biased
      ]
    ]
  ]

  set received-messages []
  ;; scale our display colour according to belief value (0 = red, 1 = green)
  set color rgb (255.0 * (1.0 - belief-value)) (255.0 * belief-value) 0
end

to decide-random
  let mysize length received-messages
  if mysize > 0[
    if old-received-weight > 0[
      let oldmessage []
      set oldmessage lput old-received-weight oldmessage
      set oldmessage lput old-belief-value oldmessage
      set oldmessage lput old-sender-id oldmessage
      set received-messages lput oldmessage received-messages
      set mysize mysize + 1
    ]
    ;set new belief and value
    let choice random mysize ;- 1
    set received-weight item 0 (item choice received-messages)
    set belief-value item 1 (item choice received-messages)
    set sender-id item 2 (item choice received-messages)
  ]  
end

to decide-average
    let total-value 0
    let total-weight 0 
    if old-received-weight > 0[
      let oldmessage []
      set oldmessage lput old-received-weight oldmessage
      set oldmessage lput old-belief-value oldmessage
      set oldmessage lput old-sender-id oldmessage
      set received-messages lput oldmessage received-messages
    ]
    foreach received-messages [
      set total-value (total-value + (item 1 (?)) )
      set total-weight (total-weight + (item 0 (?)))
    ]
    
    set total-value total-value / (length received-messages)
    set total-weight total-weight / (length received-messages)
   
    set received-weight total-weight
    set belief-value total-value
    set sender-id item 2 (item 0 received-messages)
end

to decide-mode
  if old-received-weight > 0[
    let oldmessage []
    set oldmessage lput old-received-weight oldmessage
    set oldmessage lput old-belief-value oldmessage
    set oldmessage lput old-sender-id oldmessage
    set received-messages lput oldmessage received-messages
  ]
    
  let valuelist []
  foreach received-messages [
    set valuelist lput item 1 (?) valuelist
  ]
  let modelist modes valuelist
  ifelse length modelist > 1 [
    let rand random length modelist
    set belief-value item rand modelist
  ]
  [ set belief-value item 0 modelist ]
  
  show belief-value
  ;need to find corresponding weight -- yes, I'm doing this a stupid way
  foreach received-messages [
    if item 1 ? = belief-value [
      set received-weight item 0 ?
      set sender-id item 2 ?
    ]
  ]
end

; more likely to decide on a message that has a higher weight
; information is chosen probabilistically based on weight
to decide-biased
  let bias-sum 0
  if old-received-weight > 0[
    let oldmessage []
    set oldmessage lput old-received-weight oldmessage
    set oldmessage lput old-belief-value oldmessage
    set oldmessage lput old-sender-id oldmessage
    set received-messages lput oldmessage received-messages
  ]
  
  ;find sum for random number generation
  foreach received-messages [
   set bias-sum bias-sum +  (item 0 (?))
  ]
   
  ;sort the list of received messages
  set received-messages sort-by [(item 0 (?1)) < (item 0 (?2))] received-messages
  
  ;create new list of added values for proper choosing
  let choose-list []
  let last-weight 0
  foreach received-messages [
    let newitem []
    set newitem lput (item 0 ? + last-weight) newitem ;what we'll use for choosing
    set newitem lput (item 0 ?) newitem
    set newitem lput (item 1 ?) newitem
    set newitem lput (item 2 ?) newitem
    set choose-list lput newitem choose-list
    set last-weight item 0 ?
  ]
  
  ;choose random number
  let rand random-float bias-sum
  
  ;iterate through list to determine which was chosen
  ;choose-list has each message as: [choosing value] [received-weight] [belief-value]
  ;therefore item numbers are 1 larger than are usually used on message lists
  foreach choose-list [
   if rand < item 0 ?[
     set received-weight item 1 ?
     set belief-value item 2 ?
     set sender-id item 3 ?
     set rand bias-sum + 1 ;guarantees only the first time the equality is true it will count
   ] 
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
265
10
828
594
20
20
13.5
1
10
1
1
1
0
0
0
1
-20
20
-20
20
1
1
1
ticks

BUTTON
25
88
233
121
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
26
125
84
165
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

PLOT
4
375
259
539
believers
time
% of nodes
0.0
52.0
0.0
100.0
true
true
PENS
"has-belief" 1.0 0 -10899396 true
"true-belief" 1.0 0 -16777216 true

SLIDER
25
15
230
48
number-of-nodes
number-of-nodes
10
300
145
5
1
NIL
HORIZONTAL

SLIDER
26
181
231
214
initial-observer-count
initial-observer-count
1
number-of-nodes
3
1
1
NIL
HORIZONTAL

SLIDER
25
50
230
83
average-node-degree
average-node-degree
1
number-of-nodes - 1
7
1
1
NIL
HORIZONTAL

BUTTON
175
126
230
166
step
step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
27
224
231
257
initial-liar-count
initial-liar-count
0
number-of-nodes
30
1
1
NIL
HORIZONTAL

BUTTON
91
126
164
165
NIL
gossip
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

CHOOSER
27
272
191
317
Decision-Rule
Decision-Rule
"Random" "Average" "Mode" "Weight Biased"
3

PLOT
872
26
1072
176
Fitness plot
Fitness
Number of Nodes
0.0
1.0
0.0
145.0
true
false
PENS
"default" 0.1 1 -16777216 true
"fitness-pen" 0.1 1 -16777216 true

SWITCH
28
325
154
358
spread-truth
spread-truth
0
1
-1000

@#$#@#$#@
GOSSIP
-------

   "Honesty is such a lonely word."

How do norms of strategic truth and lie telling evolve?

Fun payoff functions:

* give everyone a secret preference
* payoff shared by folks who work out the truth

OUTPUT
-------

When you run the code (with or without behavior space) you will be asked for a filename.  This file will have output the average fitness per event number.

KNOWN BUGS
----------
Check out the line

let this-weight (([received-weight] of myself) * outgoing-weight)

Warning - this takes forever to decay for large link strength, as close neighbours chatter endlessly about the same incident.


CHANGELOG
-------

06/24/10:
To run through BehaviorSpace:
- set time limit to 0
- set stop condition to "event-number = X", with X meaning how many events you want to run
- set final command to "file-close"
- uncheck "Measure runs at every step"
- Measure runs using these reporters: "allfitness" (this part is no longer necessary, you can leave empty

;;NOTE ABOUT CODE:
;; "TODO:" means something we need to do before we can run the model
;; "TODO: (distant in future)" means something we want to do but should be one of the very last things done

Changes from 06/14/10: 
- Threshold determines when message stops propagating by agents
- Weight of message (based on previous weights) is passed to reciever of message
- Initialization of agents and becoming uninformed are separated; one is used for setting up, one is used after propagating a message
- Weight of message that is sent is based on weight received with message and weight on link between the nodes
- Simulation ends once no more messages are being sent (determined by use of global_sent? variable)
- Written TODO notes throughout functions so we know where we need to make additional changes
- Added comments

Changes from 06/14/10
;;;; 
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>file-close</final>
    <exitCondition>event-number = 100</exitCondition>
    <metric>allfitness</metric>
    <enumeratedValueSet variable="average-node-degree">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="145"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Decision-Rule">
      <value value="&quot;Random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-observer-count">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-liar-count">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
