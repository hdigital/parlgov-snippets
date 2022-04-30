---
date: "2010-09-28"
title: ParlGov II -- database refactoring
---

The second major change for the next ParlGov version will be a major refactoring of our database design. The causes for the need to refactor the structure of our database are similar to the previously described plans to relaunch the web design. Some of the fundamental design decisions were made at the early stages of the project and limit its growth now. Over time, our experience and technical expertise has grown and we are convinced that the benefits of a redesign outweigh the (transformation) costs.

Most fundamental will be the overhaul of all IDs. In the next version or ParlGov, there will be newly defined identifiers for all observations, most importantly for countries, parties, elections and cabinets. Previously, we had unique identifiers per country only and some of the ID variables were based on other observations and contained meaningful information. We will change that to a system of IDs were all identifiers are unique for every variable and are just numerical identifiers without containing other information. In database terminology, we are moving from a system of composite (natural) primary keys to (surrogate) primary keys.

There will also be a clean-up for some table and variable names and the new names are often more explicit than the previously used shortened versions.  In addition, we are changing from camel case VariableNames to underscore variable_names. Finally, there are some updates on the table structure.

These modifications require users of ParlGov to  update the scripts they have written to work with ParlGov data. We are convinced that it will be for the better. The new database design is easier to understand and allows us to integrate innovative aspects into future versions more easily.

![](/images/parliament-sweden.jpg)
