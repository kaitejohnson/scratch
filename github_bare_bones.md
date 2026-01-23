GitHub cheat sheet

Typical workflow for version-controlled, PR/issued workflow:

1. Make an issue on GitHub 
2. Pull main onto local computer, make a new branch named by the issue number

  `git checkout main`
  `git pull`
  `git checkout -b <issuenumber-shorthanddetails>`
  
  3. Start making code changes to the branch and committing them 
    `git status`: red are things that haven't been staged for commit, green are things that have been staged already (e.g. already added)
    `git add R/my_new_function.R`
    `git commit -m "added new function"`
  
  4. Once you've done a few things, push the branch
  
  `git push --set-upstream origin <issuenumber-shorthanddetails>`
  
  5. Open a draft PR in GitHub 
  
  6. Continue working on the branch, until you feel ready for the changes to be "reviewed". This review could be done by yourself, or by coderabbit, or by me or a collaborator.
  
  7. When approved, merge the PR on GitHub and delete the branch. 
  
  Things to note: 
  - Can work on multiple things at once, and as long as they are not touching the exact same code GitHub will know how to separate them. 
  - If working on a branch with an open PR and something else gets merged to main, click "Update Branch" on GitHub and that will merge the updates from main into your branch. This will then require that you `pull` those updates to your branch with a `git pull`. Alternatively, you could directly merge main with `git merge main`. You will then need to make a note of the merge which will open in terminal automatically. For no messages, just type `:wq` (write and save). 
  - You should probably start projects with a template which contains some GitHub actions in CI, and set up some form of `pre-commit` hooks, which basically will lint and style your files before you push. I can show you an example and you can always find one from other projects by navigating the commit history. 
  
  There are a tonnnn of other things to do with git in the command line, these are just the basics. Definitely worth going through a [tutorial](https://product.hubspot.com/blog/git-and-github-tutorial-for-beginners) to set up your `gh auth login` etc so everything is smooth. 