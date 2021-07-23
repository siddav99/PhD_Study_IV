- The Y-axis on your plots, for example plot-1.Mobility.jpeg, says linear prediction. The linear prediction is typically the linear combination of the coefficients, on the log odds scale. The easiest way to fix this is probably to modify the plot directly, and there seems to be a scales argument to emmip that might do the trick. Check out the scales package and see if there's an appropriate transformation already, from log odds to odds perhaps.
SD: Tried the scales package and the scale function in emmip scales = exp_trans(), ylim = c(0, 2) or scales = log10_trans(). It doesn't seem to work. Tried a lot combinations. I have tried using a stacked graph, using the package effects

- When you do `exp(cbind(OR = coef(m.model3), m.model3.ci))` you're just printing this to the standard output, i.e. console. Is that what you're trying to achieve? Or are you trying to modify the coefficients in `m.model3`?
SD: Just to get an overview in a single view

- Overall this looks very good. At some point you will need to start storing the results you want to display in text in some object or list or whatever, so that you can retrieve them using code. For example, you might want to store the odds ratios from `m.model3` in `m.model3.or` or similar. 
SD: Sure, done that.

