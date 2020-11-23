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



