---
title: Structure
category: Explanations
categoryindex: 3
index: 1
---

# Structure of ImageProcessing

---

## Core Concepts

### 1. DataTable
Lists objects and the rules between them. Consists of several "columns": object name, object variants to choose from, rules describing the behaviour of the object and the expected amount of free space around it, and possible child objects that can be placed around the parent.

### 2. Cell
Cell defines the smallest possible unit of a room for arranging furniture. It has several basic states such as Occupied, NonOccupied or AgainstTheWall, which can be extended to give a more accurate furnishing result.

### 3. CellGrid
Describes the available room space using cells and their positions.

### 4. Room
Encapsulates a room using its width, height, floor number as seed for random generator, placement function and data table.

## Diagram

Take a look at the UML class diagram:

![image](https://raw.githubusercontent.com/PolinaSavelyeva/RoomInteriorGenerator/docsSrc/resources/uml.png)
