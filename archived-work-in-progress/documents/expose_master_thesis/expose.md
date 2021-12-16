
# Exposé Master Thesis

### Aim
To help produce forecasts of cases, effective reproductive numbers, intensive care bed needs and other useful quantities in the current Covid-19 pandecmic. 

### Methods and Project Parts

#### Forecasting
- explore different methods and model classes to produce probabilistic forecasts
    - ARIMA models
    - State Space Models
    - Bayesian Structural Time Series
    - Neural Networks
    - partly mechanistic epidemiological models (forecast a trajectory of $R_t$, the average number of people infected by each ill individual and use a mechanistic model to generate case predictions from that)
- explore ways to find appropriate windows for the data to be included and sensible forecast horizons
    - might involve change point detection or optimising predictive scores

#### Evaluation
- look at sensible proper scoring rules for probabilistic forecasts
- determine a frame work to sensibly make model choices

#### Model Stacking and post-processing
- combine models to a reasonable ensemble
    - find an appropriate method, e.g. non-homogeneous regression, stacking and implementation
- find a way to do post-processing of forecasts to achieve better calibration

    
