`data-public/` File Server Directory
=========

This directory should contain only datasets that DO NOT hold [PHI](https://www.hhs.gov/answers/hipaa/what-is-phi/index.html) (Protected Health Information), or any other sensitive information.  We recommend using an enterprise database (such as SQL Server, PostgreSQL, MySQL, or Oracle) to store the data, and read & write the information to/from the software right before and after it's used.  These databases typically secure the information at rest, and then require user authentication/authorization (to reduce the chance of sensitive information being accessed by those not approved by your [IRB](https://en.wikipedia.org/wiki/Institutional_review_board)).

If a database isn't feasible, consider storing the files in `S:/.../{project-directory}/data-private/`, whose contents are not committed to the repository; a line in the `.gitignore` file keeps the files uncommitted/unstaged.  However, there could be some information that is sensitive enough that it shouldn't even be stored locally without encryption (such as PHI).
