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

