# Graph COP Community Coding Sessions

This is a repo dedicated to community coding sessions in which we get together and trouble shoot problems related to graphs. 

- **Code:** Contains code used during the session
- **Data:** Contains data either sythesized during a session or obtained to be used during a session. 
- **Images:** Contains images and gifs meant for reporting

---

### Installing local packages

`devtools::install("./path/to/package")`

or

`install.packages("./path/to/package/root/dir", repo = NULL, type = "source")`

--- 
### Getting familiar with the code

The infection.graph package uses modules to extend a network based discrete time SIR model.


Modules have two components:

init - how should this module modify initial conditions

next - how should this module modify conditions in the next time step

Modules are combined to create an infection model which is then run on a particular network (g).

The network is either generated synthetically or created by surveying individuals in group. (plan to extend with ergm) 

To get the central tendancies of system, we run infection model hundreds or thousands of times to create a distribution outcomes.

### Summary of Approach

SARS-COV-2 infections result from interactions at fine spatial and temporal scales; however, case clusters occur within a larger scale epidemiological context. Appropriately integrating the interactions between processes that occur across spatial and temporal scales is essential for simulating systems of disease transmission and understanding infection risk. Here we present a simulation engine that places a fine scale network based infection model within the broader epidemiological context of the study population. Using this system, we are able to simulate seeding events and fully explore the control strategy space.

Our simulation engine can integrate a spatially explicit COVID-19 case estimation technique at the county scale with institution level disease transmission at a “building” scale. The case estimation technique takes into account location specific factors around infection control and population level movement to estimate disease burden in a given location. The institution level model uses a multigraph to integrate social and spatial contact networks under various hazard reduction strategies at two different time scales. This allows us to model individual level interactions in the local context of the COVID-19 pandemic, opening a closed system to external forces.

Our model provides realistic estimates of SARS-COV-2 outbreaks and the ability to discover novel control strategies. By integrating models at multiple scales, our simulation engine empowers decision makers to develop location specific preparedness policies based on realistic estimates of how SARS-COV-2 will spread through their institutions. 

![model diagram](https://github.com/PlusOneData/NetworkInfection/blob/master/ComplexSystemsAbstract/modelDiagram.png?raw=true)


