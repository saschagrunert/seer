# seer 🔮
[![Build Status](https://travis-ci.org/saschagrunert/seer.svg)](https://travis-ci.org/saschagrunert/seer) [![Build status](https://ci.appveyor.com/api/projects/status/1bjn2k6rjlsldu0b?svg=true)](https://ci.appveyor.com/project/saschagrunert/seer) [![License MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/saschagrunert/seer/blob/master/LICENSE) [![Development Status](https://img.shields.io/badge/status-in%20development-yellow.svg)](#about)

## A collaborative resource planning tool

Table of contents:

- [About](#about)
- [Installation](#installation)
- [Usage](#usage)
- [Hacking](#hacking)
- [Contributing](#contributing)

---

## About
A lots of development teams and whole companies facing the problem of planning
their time in an efficient manner. This can have various reasons, like too many
ongoing work or unclear requirements from customers. This ends up mostly in not
having any clear vision at all which affects the daily development and will lead
into bad software quality.

If there is no clear vision it is hard to develop a good roadmap. If there is no
good roadmap it is hard to do the weekly planning.

**This is where the seer comes in.**

## Installation
The project is currently under development which means there is no official
release yet.

## Usage
After the installation Seer can be used via the command line:

```console
> seer
seer - A collaborative resource planning tool

Usage: seer COMMAND [--version]

Available options:
  --version                Print the current version
  -h,--help                Show this help text

Basic commands:
  config                   Configure the environment
  get                      Display one or many entities
  create                   Create an entity
  delete                   Delete an entity
  edit                     Edit an entity
  view                     View the Agenda

More info at <https://github.com/saschagrunert/seer>
```

### Storages
#### Create a Storage instance
The first thing to be done is to create a storage instance.

```console
> seer create storage myLocalStorage
✓ Done
```

Optionally it is possible to provide a remote location for the storage, which
should be an empty git repository.

```console
> seer create storage myStorage -r git@github.com:saschagrunert/seer-storage.git
✓ Done
```

#### Listing Storages
The get an overview about the available Storage instances simply execute:

```console
> seer get storages
NAME            REMOTE
myLocalStorage
myStorage       git@github.com:saschagrunert/seer-storage.git
```

### Config
Per default, the last created storage is currently selected. The Config can be
changed to select another Storage:

```console
> seer config get
#  STORAGE    CREATED
1  myStorage  01.04.18 12:26

> seer config set-storage myLocalStorage
✓ Done

> seer config get
#  STORAGE         CREATED
1  myLocalStorage  01.04.18 12:26
```

### Resources
#### Creating Resources
One of the basic entities of Seer are Resources. A Resource can be anything you
can imagine which has an available amount of time. For example, team members are
resources, but something like rental cars can be resources too. To create a
resource called "me" which is available on Mondays from 10:00-18:00 and Fridays
from 10:00-15:00, execute:

```console
> seer create resource me -m 10-18 -f 10-15
✓ Done
```

Generally, Resources have a name, an optionally description and a weekly
availability. All options can be seen using the help:

```console
> seer create resource -h
The option `-h` expects an argument.

Usage: seer create resource NAME [-d|--description DESCRIPTION]
                            [-m|--mon HH[:MM]-HH[:MM]]
                            [-t|--tue HH[:MM]-HH[:MM]]
                            [-w|--wed HH[:MM]-HH[:MM]]
                            [-h|--thu HH[:MM]-HH[:MM]]
                            [-f|--fri HH[:MM]-HH[:MM]]
                            [-s|--sat HH[:MM]-HH[:MM]]
                            [-u|--sun HH[:MM]-HH[:MM]]
  Create a new Resource within the default Storage

Available options:
  -d,--description DESCRIPTION
                           Optional description for the Resource
  -m,--mon HH[:MM]-HH[:MM] Monday availability
  -t,--tue HH[:MM]-HH[:MM] Tuesday availability
  -w,--wed HH[:MM]-HH[:MM] Wednesday availability
  -h,--thu HH[:MM]-HH[:MM] Thursday availability
  -f,--fri HH[:MM]-HH[:MM] Friday availability
  -s,--sat HH[:MM]-HH[:MM] Saturday availability
  -u,--sun HH[:MM]-HH[:MM] Sunday availability
  -h,--help                Show this help text
```

#### Listing Resources
All available Resources can be listed using:

```console
> seer get resources
#  NAME  DESCRIPTION  Mon          Tue  Wed  Thu  Fri          Sat  Sun  CREATED
1  me                 10:00-18:00                 10:00-15:00            01.04.18 13:13
```

#### Editing Resources
Resources can be edited too, like this:

```console
> seer edit resource me -m 10-19 -f 10-15
✓ Done
```

Now the Resource will be updated:

```console
> seer get resources
#  NAME  DESCRIPTION  Mon          Tue  Wed  Thu  Fri          Sat  Sun  CREATED
1  me                 10:00-19:00                 10:00-15:00            01.04.18 13:13
```

### Actions
#### Creating Actions
The second base entities are Actions. An Action can be anything you can imagine
which consumes time. For example, tasks are actions. To create an action called
"myTask" which takes two hours time execute:

```console
> seer create action myTask 2h
✓ Done
```

Generally, Actions have a name, an optionally description and a duration. All
options can be seen using the help:

```console
> seer create resource -h

Usage: seer create action NAME DURATION [-d|--description DESCRIPTION]
  Create a new Action within the default Storage

Available options:
  DURATION                 The time the action will take, like '1y', '2w', '3d',
                           '4h', '5m'
  -d,--description DESCRIPTION
                           Optional description for the Action
  -h,--help                Show this help text
```

#### Listing Actions
All available Actions can be listed using:

```console
> seer get actions
#  NAME    DESCRIPTION  DURATION  CREATED
1  myTask               2h        29.04.18 11:49
```

#### Editing Actions
Actions can be edited too, like this:

```console
> seer edit action myTask -r 1h
✓ Done
```

Now the Action will be updated:

```console
#  NAME    DESCRIPTION  DURATION  CREATED
1  myTask               1h        29.04.18 11:49
```

### Schedules
#### Creating Schedules
The last base entities are Schedules. Schedules glue Actions and Resource
together to a real time plan. To create an Schedule between the Action "myTask"
and the Resource "me", execute:

```console
> seer create schedule now me myTask
Using calculated date range: 30.04.18 10:00 - 30.04.18 11:00
✓ Done
```

Seer automatically selects the next available time slot for this task, if not
already blocked by another task.

Generally, Schedules have a Resource, an Action and a starting date. All options
can be seen using the help:

```console
> seer create schedule -h
Usage: seer create schedule START RESOURCE ACTION
  Create a new Schedule within the default Storage

  Available options:
    START                    The date and time when the Schedule starts
    RESOURCE                 The name of the Resource which should be used
    ACTION                   The name of the Action which should be done
    -h,--help                Show this help text
```

The start date parser supports absolute dates (in EU and US format) and relative
dates (like "next monday", "now").

#### Listing Schedules
To get an overview about all Schedules, execute:

```console
> seer get schedules
#  FROM            TO              Σ   RESOURCE  ACTION  CREATED
1  30.04.18 10:00  30.04.18 11:00  1h  me        myTask  29.04.18 11:55
```

Alternatively a more agenda based view is available too:

```console
> seer view
Mon [30.04] 10:00 ↦ myTask (me)
            11:00 ⇥ myTask (me)
```

#### Editing Schedules
Schedules can be edited too. So it should be possible to reschedule if the
time slot is available, change the Action or even the Resource of the Schedule.

For example, to change the Schedule with the number `1` (referenced from the
`seer get schedules` command) to the next 5th May:

```console
> seer edit schedule 1 -s "7. may"
✓ Done
```

Now the Schedule should be changed as intended:

```console
> seer get schedules
#  FROM            TO              Σ   RESOURCE  ACTION  CREATED
1  07.05.18 10:00  07.05.18 11:00  1h  me        myTask  29.04.18 11:55
```

## Hacking
To start hacking simply clone this repository and make sure that
[stack](https://docs.haskellstack.org/en/stable/README/) is installed. Then
simply hack around and build the project with:

```console
> stack build --file-watch
```

## Contributing
You want to contribute to this project? Wow, thanks! So please just fork it and
send me a pull request.
