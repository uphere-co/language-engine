HFrameNet: FrameNet parsing library and querying utilities

To build query program:
```
$ nix-shell shell.nix
$ cabal sandbox int
$ cabal install
```

Look up frames
--------------

```
$ .cabal-sandbox/bin/fnquery frame -d (base directory for FrameNet)
```
For example,
```
$ .cabal-sandbox/bin/fnquery frame -d /scratch/wavewave/fndata/fndata-1.7
% Intentionally_act
<def-root>This is an abstract frame for acts performed by sentient beings. It exists mostly for FE inheritance.

<ex><fex name="Agent">I</fex> <t>carried</t> <t>out</t> <fex name="Act">the deed</fex> <fex name="Manner">easily</fex> .</ex></def-root>
============================
Inherits from
----------------------------
Event
============================
Is Inherited by
----------------------------
Name_conferral  Intentionally_affect    Avoiding        Intentionally_create    Ingest_substance        Choosing        Self_motion
Perception_active       Activity_finish Practice        Heralding       Get_a_job       Quitting        Clemency
Exchange        Change_tool     Resolve_problem Intercepting    Forming_relationships   Collaboration   Change_posture
Bail_decision   Assistance      Atonement       Daring  Execute_plan    Becoming_a_member       Giving
Front_for       Examination     Piracy  Passing_off     Hostile_encounter       Visiting        Confronting_problem
Legal_rulings   Working_a_post  Military_operation      Assemble        Attempting_and_resolving_scenario       Attending       Exercising
Contacting      Manipulation    Attempt
============================
Is Used by
----------------------------
Competition     Bungling        Rite    Conduct Accomplishment  Reason  Assistance
Deciding        Remembering_to_do       Waiting Subjective_influence    Experience_bodily_harm  Responsibility
============================
============================

%
```

Look up lexical units
---------------------

```
$ .cabal-sandbox/bin/fnquery lu -d (base directory for FrameNet) +RTS -N -I0
```
Here, `-N` is for parallel loading and `-I0` is for disabling idle-time garbage collection which renders
the program significant slow for interactive use.



```
$ time .cabal-sandbox/bin/fnquery lu --dir /scratch/wavewave/fndata/fndata-1.7 +RTS  -s  -I0
...
% id 1234
============================
    id:    1234
  name:               step.n
   POS:     N
 frame:          Self_motion
lexeme:               step N
definition: FN: a motion of the foot and leg which results in overall motion of a body.
---- example sentences -----
- She took a few steps about the room , hoping the movement successfully concealed her dread .
- He had taken only a few steps across the playground when he was stopped by a rough command .
- This time , we succeeded and indeed met no difficulties apart from one awkward step across a gap just
before the cairn .
============================
% name look.v
============================
    id:     447
  name:               look.v
   POS:     V
 frame:             Scrutiny
lexeme:               look V
definition: COD: attempt to find.
---- example sentences -----
- We ran the length of the train looking for an unlocked door .
- Some of the sailors and I managed to get a boat into the water , and we rowed away to look for land .
- Would he range the world looking for sheep to care for if there were none here ?
============================
============================
    id:    1323
  name:               look.v
   POS:     V
 frame:      Give_impression
lexeme:               look V
definition: FN: appear
---- example sentences -----
- THERE are still bargains to be found among the terraced property and this one on the books of J Trevor & Sons looks to be worthy of closer inspection .
- This looks to have been painted a long time ago . 
- Of the other main contenders , Wasps got the better of Harlequins in their league opener , but neither looks sharp enough to worry Bath this year . 
============================
============================
    id:    1302
  name:               look.v
   POS:     V
 frame:    Perception_active
lexeme:               look V
definition: COD: direct one's gaze in a specified direction
---- example sentences -----
- He scrabbled into a half run , going deeper into the Wolfwood , not looking where he was going , not caring overmuch . 
- The baby was now cleaned up and Sarah looked down into the bluest eyes she had ever seen . 
- He then looked about , expecting to see a cage with a dead bird , the body of a starved cat in a basket . 
============================
% 
```