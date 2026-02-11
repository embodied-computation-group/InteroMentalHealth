
# Estimated psychophysical parameters of the HRD.

## HRD

Hierarchical_HRD_Intero.csv and Hierarchical_HRD_extero.csv contains the subject level estimates of the hierarchically fit psychometric function for the VMP dataset.
We used the cummulative normal with the following parameterization of the three parameters (lapse, threshold (alpha) and slope (beta)).

lapse + (1 - 2 * lapse) * (0.5+0.5*erf((X-alpha) / (beta*sqrt(2))))

As the slope is constrained to be positive its estimated in log-scale and has to be exponentiated in the data to plot the psychometric functions. 
The lapse is bounded between (0,0.5) and thus the transformation is the inverse logit but devided by 2 i.e. (brms::inv_logit_scaled(.) / 2).

In these .csv files some summary statistics are also included from the task.

## RRST

Hierarchical_RRST_subjectlevel_estimates.csv contains the subject level estimates of the hierarchically fit psychometric function for the VMP dataset.
We used the gumbel normal with the following parameterization of the three parameters (lapse, threshold (alpha) and slope (beta)).

0.5 + (1 - 0.5 - (lapse )) .* (1-exp(-10^(exp(beta)*(X-alpha))))

As the slope is constrained to be positive its estimated in log-scale and has to be exponentiated in the data to plot the psychometric functions. 
The lapse is bounded between (0,0.5) and thus the transformation is the inverse logit but devided by 2 i.e. (brms::inv_logit_scaled(.) / 2).

