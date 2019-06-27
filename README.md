# Pokémon Emerald: Nuzlocke Mod

This is a mod for Pokemon Emerald built using pret's pokeemerald decompilation. It turns the game into a "Nuzlocke" challenge, with the rules enforced by the game rather than the player.

## Features:
* Only the first Pokemon you encounter in a route is catchable. If you attempt to catch a Pokemon on a subsequent encounter, the game will stop from from throwing a Ball.
* When a Pokemon faints, it is gone forever. During the battle in which it fainted, it cannot be revived. At the end of the battle, all fainted Pokemon are permanently deleted. Any items held by fainted Pokemon are returned to the bag before they are deleted, so you will not lose your items. If you run out of all of your Pokemon and white out, the game will search through your Boxes and move the first Pokemon it finds into your party. If you have no Pokemon in your Box when you white out, the game will soft reset.
* A "species clause" has been implemented; if your first encounter is a Pokemon in the same evolutionary line as a Pokemon you have already caught, you will not be allowed to capture it, and your encounter for the area will not be used.
* A "shiny clause" is also in effect which overrides everything. If a Pokemon is shiny, you are allowed to capture it no matter what.
* The Nuzlocke run is controlled by a global switch that turns ON when you first receive the Pokedex and turns OFF when you beat Steven in Meteor Falls. This way, your first encounters on Route 101 and Route 103 are not wasted before you can capture them, and after you have beaten Steven, you can play the game as you normally would, without the restrictions of a Nuzlocke.
* HMs no longer need to be taught to Pokemon to be used on the field; the game now checks if you have the item and the appropriate badge instead. This is because it would be possible to get stuck or be unable to progress if Pokemon who knew HMs died, and it would be unnecessarily complicated to implement an "HM slave" system into the game. Additionally, moves such as Flash and Fly that require you to use the Pokemon status screen to select the move have been turned into items; the NPCs who give you the HMs for Flash and Fly now also give you key items simply named FLASH and FLY that work identically to the regular field moves.
* Not directly related to the Nuzlocke, but the RNG is now properly seeded from the RTC, so you won't get RNG clones anymore.

## Building
Building is identical to building regular pokeemerald. If you are not familiar with the build process, look at the [pret repository](https://github.com/pret/pokeemerald).
