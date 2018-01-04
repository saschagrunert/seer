# Requirements

This documents purpose is to document the requirements for every single major
version of the application.

## Version 0.1.0

### Guide level

It should be possible, to …

1. …save the applications state persistently.
2. …work collaboratively with other Users of the application.
3. …manage a 'Team' by adding or removing 'Users'.
4. …add 'things' to certain or multiple Users of the Team.
5. …display the resource usage of certain teams and Users.
6. …display the schedule for a chosen time interval.
7. …customize the actual work time slots.

### Reference level

- The main application will be deployed as command line tool.
- git repositories will be used as backend to save the applications state. This
  abstracts the network communication, file handling and data storage. Multiple
  Users are able to collaborate out of the box this way.
- Caching of the application state will be done within the local
  `~/.cache/seer/` directory
- The configuration management will be done within a (per repository) `yaml`
  configuration file
- The entities/projects/tasks/things (aka 'Action's) will also be stored as
  `yaml` files within the git repository.
