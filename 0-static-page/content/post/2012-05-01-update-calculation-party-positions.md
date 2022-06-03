---
date: "2012-05-01"
title: Update calculation party positions
---

ParlGov party positions are based on various party expert surveys (Castles/Mair 1983, Huber/Inglehart 1995, Ray 1999, Benoit/Laver 2006 and CHESS 2010). These positions are just the mean value of all observations available from these sources in a particular dimension (left_right, state_market, liberty_authority, eu_anti_pro). 

There was a bug in the scripts that converted the data from Benoit/Laver and not all positions from the data set were included into the calculation. We became aware of the issue, when someone pointed out the extremely moderate left/right position of the German far-right NPD in ParlGov. We have now solved the issue and include data on all relevant dimensions from Benoit/Laver into the calculation. This also solves the issue of having information in the left/right dimension for some minor parties instead of using the state/market dimension.

While updating the position data we have also renamed the variables for the EU dimension from ‘eu_pro_contra’ into ‘eu_anti_pro’ since lower values on the scale present eurosceptic positions.

![](/images/parliament-scotland.jpg)
