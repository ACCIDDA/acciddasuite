# Disease Forecast Planning for Public Health

## Introduction

Disease forecasting is an invaluable tool for the field public health,
however, clearly defining the parameters of the problems you want to
solve is often more difficult than the forecasting itself. The
`acciddasuite` pakcage provides a comprehensive toolkit for generating
forecasts with the expectation that users already have a concrete idea
of what they would like to forecast. In this vignette, we provide a
series of questions to aid forecasters in the discovery of the “why”,
“what”, “where” and “how” for their question. This package and its
documentation is still actively under development, and we welcome
[contributions and
feedback](https://github.com/ACCIDDA/acciddasuite/issues/new) from the
community.

## Step 1: Why are we interested in forecasting?

First, there needs to be a clearly defined project to get started. Here
are a set of questions to consider to begin this process:

1.  What is the question that you are attempting to answer? Or what
    insights do you hope to gain?

    - Determine what you are trying to gain from the forecasting
      project. Specificity at this stage makes future stages easier!

2.  Who is the audience, or, who will benefit from the insights?

    - Determine who will use the forecasts, and who the interpretation
      of them will benefit.

3.  How far into the future are you interested in forecasting? How far
    into the future do these insights need to be to be useful?

    - \<7 days? 1-4 weeks? Full seasonal projections\*?
      - \*full seasonsal projections are known as “scenarios” and are
        different from forecasts.
    - The time frame associated with your question determines whether or
      not forecasting is the best tool. Read more in Step 2…

STOP! Have you defined your forecasting problem using the questions
above?

- YES → Proceed to next step
- NO → Continue defining the approach

## Step 2: Forecast vs. scenario?

Determine if your central question is strictly a forecast, or if it is
more aligned with a scenario projection.

- Forecasts
  - Forecasts are concerned with what will happen in the future under
    current conditions/regardless of what interventions take place. From
    this, we can determine actionables or deploy resources, but our
    starting point is based off of “real life” and not an assumption
    (i.e., an unconditional projection).
  - e.g., I want advanced notice on the influenza hospitalization burden
    in the coming weeks.
- Scenarios
  - Scenarios are concerned with what will happen if we take action *X*?
    Or, how will the future differe if *X* happens instead of *Y*?
    Often, with scenarios, you are comparing different outcomes on the
    basis of different assumptions (i.e., a conditional projection).  
  - e.g., I want to know what the predicted burden will be if 50% of the
    population is vaccinated against flu vs. only 30%.

If you want to produce projections of the future more than a few weeks
at a time, your question is likely better suited for a scenario
projection rather than a forecast (forecasts give us a look 1-4 weeks
ahead of any given start date, scenarios show us entire seasons at a
time). For more information on scenario projections, visit the [Scenario
Modeling Hub](https://scenariomodelinghub.org/).

STOP! Have you confirmed that your question is best answered with a
forecast?

- YES → Proceed to next step
- NO → Consider checking out scenario projections instead!

## Step 3: Define Your Data

Next, define what **pathogen**, **target** (data stream), and **spatial
unit** are involved in forecasting project.

1.  What **pathogen** are you interested in forecasting?

    - e.g., influenza, COVID-19, RSV, etc.

2.  What data stream are you interested in forecasting (also known as
    your **target**)?

    - Examples of common forecasting targets are “incidence of
      hospitalization” (of patients with a certain hospitalization),
      “percent of emergency department visits” (attributable to a
      certain pathogen), “deaths” (due to a certain pathogen), “hospital
      bed occupancy” (by patients with a certain pathogen). All of these
      targets are measures of disease burden that give public health
      professionals an idea of how much disease a population bears at a
      given time. The most common forecasting target for respiratory
      illnesses is “incidence of hospitalization”, which makes it the
      easiest to find data on. Presently, `acciddasuite` only forecasts
      targets “incidence of hospitalization” and “death”.

3.  What **spatial unit** will provide the best insight? Is there data
    available at that scale?

    - e.g., national, state, county, city, health jurisdiction, hospital
      system, or even facility (e.g., hospital). The more granular the
      spatial unit, the more difficult it is to find data, so there is
      often a trade-off between data specificity and availability.

Note that you can forecast multiple locations (e.g., multiple states or
health jurisdictions) at once, but if you want forecast multiple
pathogens or targets, it is best to separate those into their own
distinct forecasts.

STOP! Have you defined your pathogen, target, and spatial unit?

- YES → Proceed to next step
- NO → Continue defining these data elements

## Step 4: Data Availability & Limitations

In our context, forecasts are mathematical predictions of a few weeks
ahead given a starting point of “ground truth” data (i.e, information on
what has already happened). Because of this, you need to provide
forecasting models with ground truth data that matches the resolution of
your forecasting question (i.e., same pathogen, same target, same
locations). Data availability is often a limiting factor when
considering a forecasting question. In this step, we provide a decision
tree approach to determine if you can forecast with ground truth data
that already exists, if you need to provide specialized data to complete
your forecast, or if there is already forecasting infrastructure that
answers your forecasting question.

An easy way to find either ground truth data or forecasts is via a
*forecast hub*. Forecasting hubs (organized by the
[hubverse](https://hubverse.io/)) are standardized repositories for
disease forecasts and ground truth data where all data follows
structured guidelines. Ground truth data found in forecasting hubs is
forecast-ready, and in fact, the forecasts themselves may answer your
forecasting question(s) without any further action from you. For
example, if you are forecasting RSV, COVID-19, or influenza at a U.S.
national or state level\*, your forecasting question is likely already
answered by a forecasting hub:

- RSV: [GitHub repository](https://github.com/CDCgov/rsv-forecast-hub)
  \| [RespiLens
  visualization](https://www.respilens.com/?view=rsv_forecasts)
- COVID-19: [GitHub
  repository](https://github.com/CDCgov/covid19-forecast-hub) \|
  [RespiLens
  visualization](https://www.respilens.com/?view=covid_forecasts)
- Influenza: [GitHub
  repository](https://github.com/cdcepi/FluSight-forecast-hub) \|
  [RespiLens
  visualization](https://www.respilens.com/?view=flu_forecasts)
  - \*There is also a hub for sub-state level influenza forecasts
    ([GitHub repository](https://github.com/reichlab/flu-metrocast) \|
    [RespiLens
    visualization](https://www.respilens.com/?view=metrocast_forecasts))

For a complete list of hubverse forecast hubs, [see
here](https://hubverse.io/community/hubs.html).

### Finding your data

After you decide to create your own forecasts and find a suitable ground
truth data, you must confirm that your ground truth data stream is
stable enough to support a repeatable workflow.

If you are pulling ground truth data from a hubverse hub, this data is
likely released on a weekly cadence and is mostly complete. You can find
this data in the `target-data/` directory of the hub’s GitHub
repository. Alternatively, `acciddasuite` has a built-in function
(**[`get_data()`](https://accidda.github.io/acciddasuite/reference/get_data.md)**)
that will handle the collection + formatting of state level respiratory
data. If you want to use another ground truth data source, you will
first have to validate it with
**[`check_data()`](https://accidda.github.io/acciddasuite/reference/check_data.md)**.
Please see the [external
data](https://accidda.github.io/acciddasuite/articles/external_data.md)
article for information on external data source formatting.

If there are reporting delays in your data stream, or inconsistencies
that are often fixed later but you cannot wait on, use the
**[`get_ncast()`](https://accidda.github.io/acciddasuite/reference/get_ncast.md)**
function to correct recent weeks for reporting delays.

### Confirming your data

## Next steps:

When you are ready to begin, visit the [**GET
STARTED**](https://accidda.github.io/acciddasuite/articles/acciddasuite.md)
page to use `acciddasuite` for your forecasting needs!
