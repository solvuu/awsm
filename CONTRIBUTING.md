This is open source software maintained by Solvuu, Inc. We welcome
contributions, but for everyone's benefit require that you sign your
contributions. This certifies your agreement with the DCO below (from
https://developercertificate.org).


## Table of Contents

- [Labels](#labels)
- [How to file an issue](#how-to-file-an-issue)
- [Code architecture](#code-architecture)
- [Coding guidelines](#coding-guidelines)
- [How to submit a Pull request](#how-to-submit-a-pull-request)
- [How to release a new version](#how-to-release-a-new-version)


## Labels

### Issue specific

- [Good-first-issue](https://github.com/solvuu-inc/awsm/labels/Good-first-issue): This issue is a good entry point in the project for new contributors

### Pull request specific

- [Help-wanted](https://github.com/solvuu-inc/awsm/labels/Help-wanted): The author of this pull request welcomes the help from other contributors

### Kind

- [Kind/Bug](https://github.com/solvuu-inc/awsm/labels/Kind%2FBug): This issue describes a problem or this contribution addresses a problem (erroneous/unintended behavior)
- [Kind/Docs](https://github.com/solvuu-inc/awsm/labels/Kind%2FDocs): This is related to a documentation change (user documentation or source code documentation)
- [Kind/Feature-request](https://github.com/solvuu-inc/awsm/labels/Kind%2FFeature-request): This issue or contribution proposes a new feature
- [Kind/To-discuss](https://github.com/solvuu-inc/awsm/labels/Kind%2FTo-discuss): Discussion needed to converge to a solution
- [Kind/Bigdata](https://github.com/solvuu-inc/awsm/labels/Kind%2FBigdata): This issue is related to the processing of big data by awsm
- [Kind/Infrastructure](https://github.com/solvuu-inc/awsm/labels/Kind%2FInfrastructure): This issue is related to the infrastructure of the project
- [Kind/Techdebt](https://github.com/solvuu-inc/awsm/labels/Kind%2FTechdebt): This issue is related to the technical debt of the codebase (choices that have been made over the years)

### Status

- [Status/0-More-info-needed](https://github.com/solvuu-inc/awsm/labels/Status%2F0-More-info-needed): More information is needed before this issue can be triaged
- [Status/0-Triage](https://github.com/solvuu-inc/awsm/labels/Status%2F0-Triage): This issue needs triaging
- [Status/1-Acknowledged](https://github.com/solvuu-inc/awsm/labels/Status%2F1-Acknowledged): This issue has been triaged and is being investigated
- [Status/2-Regression](https://github.com/solvuu-inc/awsm/labels/Status%2F2-Regression): Issue present in the current version but not in an earlier one
- [Status/3-Fixed-need-test](https://github.com/solvuu-inc/awsm/labels/Status%2F3-Fixed-need-test): This issue has been fixed and needs checking
- [Status/4-Fixed](https://github.com/solvuu-inc/awsm/labels/Status%2F4-Fixed): This issue has been fixed
- [Status/5-Awaiting-feedback](https://github.com/solvuu-inc/awsm/labels/Status%2F5-Awaiting-feedback): This issue or contribution requires feedback from other maintainers or from its submitter

### Verdict

- [Verdict/Duplicate](https://github.com/solvuu-inc/awsm/labels/Verdict%2FDuplicate): This issue or contribution is a duplicate of an already existing issue or pull request.
- [Verdict/Invalid](https://github.com/solvuu-inc/awsm/labels/Verdict%2FInvalid): This issue does report any valid problem or this contribution does not have a positive impact on the project
- [Verdict/Wontfix](https://github.com/solvuu-inc/awsm/labels/Verdict%2FWontfix): This issue reports a valid problem but the maintainers decided to not fix it


## How to file an issue

1. Search for duplicates among the [open issues](https://github.com/solvuu-inc/awsm/issues) or [open or closed issues](https://github.com/solvuu-inc/awsm/issues?utf8=%E2%9C%93&q=is%3Aissue). You can filter the results using the [labels](#labels) or looking for keywords related to your issue. If your issue is not already reported feel free to report it.
2. To help other contributors to understand your issue, it is important to provide a minimal reproducible example of the problem you encountered. This example will serve as a witness to check whether the problem has been fixed in a future pull request. It is also considered good practice to add it to the regression tests of the project. This example should be minimal, try to find the smallest version of the example that still exhibits the problem you encountered.
3. In order to reproduce the issue, do not forget to report the input you used and the verions of the dependencies (software, services, etc.).


## Code architecture

- `lib/base`: Defines some basic modules.
- `lib/codegen`: Depends on `base`. Parses botocore files and implements functions to create ocaml structures and signatures.
- `app/codegen`: Provides a CLI tool to generate code.
- `lib/unix`: Call the codegen CLI to generate ml and mli files. Does not depend on the `codegen` library.
- `lib/{lwt,async}`: Depend on `lib/unix`, and instantiate the IO module (currently called Future) with `lwt` and `async` implementations, respectively.


## Coding guidelines

### Naming

The names for types, variables, exceptions, fields, functions and modules must be informative and consistent in the whole project. For example:
- prefer `this_name` to `thisName`
- prefer `is_predicate` to `predicate` for a predicate function
- avoid names that are too long
- avoit names that are too close to each other
- avoid single letter names for meaningful objects

### Formatting

`ocamlformat` (last stable release) should be used to auto-format the code.
You can use `make fmt` to auto-format the codebase.
Please do not blindly trust this tool and check the formatted output, especially the comments and docstrings.
If the output is too weird, do not hesitate to file an issue on ocamlformat bugtracker.

### Comments

The purpose of each variable, exception, function, type, module must be documented in the corresponding mli files.
Do not hesitate to use the [documentation comments syntax](https://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html).
Please specify if a recursive function is not tail-recursive.
Please specify where exceptions are raised and caught.
Create a mli file to expose a clear and documented interface for each module.
Run `make doc` to check whether you generated new errors or warnings.

### Code quality

1. Don't unnecessarily open modules (globally).
2. Use variant types instead of exceptions whenever possible.
3. Make recursive functions tail-recursive whenever possible.


## How to submit a pull request

1. Create a git branch, the naming convention is `your-username/your-feature`
2. The commit messages should be concise but informative, avoid single-word messages like `fix`.
2. Cross-referencing issues and pull-requests: if your contribution fixes a problem reported in an issue or is related to another pull request, please cite this issue or pull request in a commit message or in the description of the pull request, the format is `#number`.
3. Write tests for the new feature added or the problem fixed, and add them to the tests base, and check that those tests are runned properly when invoking `make test`. If the pull request is related to an issue, add the minimal example of the issue as a test, if this issue does not provide such example feel free to ask for one. Run `make && make test` to compile and run the tests.
4. Write documentation. If the pull requests exports new functions in mli files, those functions must be documented in the mli files. If the interface of existing exported functions is modified, the documentation must be updated. Run `make doc` to generate the documentation from the code and check you did not generate new errors or warnings.
5. Comply with the [coding guidelines](#coding-guidelines).
6. Sign your commits.


To sign your commits, add this line to the git commit message:

```
Signed-off-by: Alice Smith <alice.smith@email.com>
```

Your real name must be used. We recommending using `git config` to set
`user.name` and `user.email`. Then you can sign automatically with
`git commit -s`.


```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.


Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```


## How to release a new version

1. Update the [Changelog file](./CHANGES.md) with a summary of the last contributions since the last release of the project.
2. Update the `opam` package of the project, it is recommended to use `dune-release`, see the [documentation](https://github.com/samoht/dune-release/blob/master/README.md).
3. Publish the release announcement on [OCaml Discourse](https://discuss.ocaml.org/).
