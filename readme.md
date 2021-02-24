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

# Install git
[Here is a link to the official install page](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
Just download the software and run it if you dont want to install via command line.

# Git Basics

- **Remote**: the remote server where our files live
- **Local**: your computer
- **Repo**: code repository 
- `clone`: used the first time you want to copy a repo from remote 
- `branch`: a divergence from the main code base. Usually used for feature development or bug fixes. Branches can come off of branches as we will see later.
- `status`: tells you if you have modified, untracked, or staged files on your local repo and where you stand relative to the remote.
- `pull`: updates your local with what is on the remote 
- `add`: prepares a file for commit
- `commit`: commit lets you track a change to a file, requires a message. These should be brief but informative. 
- `push`: moves your local commits to remote
- `checkout`: switch from one branch to another
- `merge`: join two branches

## get the remote on your computer
In your git terminal/gui navgiate to the appropriate folder on local.
`cd my/folder/for/repo`

clone the repo from remote with the proper URL into that folder. 
`git clone HTTPS:some/remote/url`

## Move files or folders into your repo if needed
Your repo folder is now being tracked by git. When you change branches files may disappear or be added based on changes others have made. 

You can add files and folders like normal, just know they will be tracked by git. 
If you are generating files you would like git to ignore, put the path in the `.gitignore` file. 

## Add files to the stage

To add files, use the following command:
`git add some/folder/fileName`

## Commit files
To locally commit the changes you have staged via add, run the following:
`git commit -m "informative but brief message"`

## Push changes to remote

To have you changes show up on the remote: 
`git push`

## Pull changes on remote to local 
`git pull`

## Check your status
`git status`
this lets you know what you have changed on your local branch. 

# Typical git workflow

*Check the branch you are working on*

- `git pull` remote changes on to local, resolve any issues
- make some changes to a file
- `git status` look at your files on local and reltionship to remote
- `git add some/folder/file` after making incremental but meaninful changes to file
- `git commit -m "brief and informative message"` those files
- `git push` changes to remote - you may be prompted to pull because of changes on remote. Do that. Deconflict the files if needed. Add and commit the deconflicted files then try your push again. 

**Commiting more frequently is better** 
[cheatsheet](https://www.atlassian.com/git/tutorials/atlassian-git-cheatsheet)


# Creating a branch of a branch
![branch of branch diagram](https://i.stack.imgur.com/6qEWk.jpg|width)
 
## While on dev branch
 `git checkout -b feature/Name dev`
 
 That extra dev at the end says create this branch off `dev`

 **dev** already exists so there is no reason to create a new branch

## For first push, set it up stream 
 `git push -u origin feature/Name`

## Ready to merge into dev? Make a pull request 
- describe the changes you made and the expected outcome
- connect to a work item
- assign a reviewer
- merge into **master**


## Reviewing code
So you got a pull request, now what?  Checkout the branch, make sure the code runs as expected. Reference the description to
the work item and make sure they align. If the code doesn't run, or the feature doesn't match the work item/description, 
talk with the person who submitted the pull request. Do not approve a pPR that does not run. After you approve the pull request,
talk with the author of the code to determine who will merge the code into `dev` and merge with a stand merge (not fast forward).

## Is dev ready for master? 

When we get a working and tested version of dev, we will create a clean `stagingBranch` off 
of `dev` that will be merged into master via a pull request. Clean means removing unnecessary files from
from the perspective of the dashboard. The reason we have this staging branch
is so that we can maintain exploratory files from the beginning of our analyses. Each
staging branch will be different eg `stagingBranch1`, stagingBranch2` etc. 


