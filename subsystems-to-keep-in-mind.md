This is a partial list of subsystems, aggregated from Figure 1.11 of
*Game Engine Architecture*, by Jason Gregory. It's just a few
subsystems I think we may want to keep in mind, whether we implement
them directly or not.

Core Systems
------------

* math library
* geometry
* debugging and logging
* movie player

Assets
------

* textures/materials
* fonts
* collidables and physics parameters
* game world/map

Low-Level Renderer
------------------

* primitive submission
* viewports and virtual screens
* texture and surface management
* text and fonts
* debug drawing

Profiling and Debugging
-----------------------

* recording and playback
* memory and performance stats
* in-game menus or console

Collisions and Physics
----------------------

* forces and constraints
* ray/shape casting (queries)
* rigid bodies
* phantoms
* shapes/collidables
* physics/collision world

Human Interface Devices
-----------------------

* game-specific interface
* physical device I/O

Scene Graph / Culling Optimizations
-----------------------------------

* spatial subdivision
* occlusion
* level of detail

Online Multiplayer
------------------

* match-making and game management
* object authority policy
* game state replication

Audio
-----

* processing/effects
* 3D audio
* audio playback/management

Front End
---------

* heads-up display
* full-motion video
* in-game cinematics
* in-game GUI
* in-game menus

Gameplay Foundations
--------------------

* static world elements
* dynamic game object model
* real-time agent-based simulation
* event/messaging system
* world loading/streaming

Game-Specific Subsystems
------------------------

* game-specific rendering
* player mechanics
* game cameras (fixed, player-follow, scripted, debug fly-through,
  etc.)
* AI (goals and decision-making, sight traces and perception, actions,
  path-finding, etc.)
    pathfinding: A*, D*-lite, incremental phi*, mtd*-lite, anytime D*, hpa*, haa*
    see http://cstheory.stackexchange.com/questions/11855/how-do-the-state-of-the-art-pathfinding-algorithms-for-changing-graphs-d-d-l/11866#11866
