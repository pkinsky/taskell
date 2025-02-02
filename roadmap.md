## Misc.

- Add more information to taskell.app
    > Update taskell.app to have more than just README.md contents. Use cases, more images, examples, etc.
    * [ ] Use case example: checklist
    * [ ] Use case example: Git controlled tasks
    * [ ] Blog posts for updates

## Refactoring

- Use Attoparsec for parsing
- Switch over to Vectors?
    > Sequence has O(log n) lookup. Vector has O(1). Vectors support mapping with index.
- Add tests for IO.GitHub
- Break up State module
- Parse checkItems Trello JSON using Aeson FromJSON rather than needing extra record type
- Remove duplication of config - currently using ini and hard-coded defaults
- Move Help modal creation into Template Haskell
- Remove `~` style sub-task complete parsing
    @ 2018-12-10
- Use Shake instead of bash script

## Bugs

- Very long words should get hyphenated
    > The cursor gets lost if a word is longer than the line - URLs in particular can cause issues
- No longer a difference between <Space> and move right
- Help modal needs to wrap and scroll
- Limit modal height based on content
- Multiple spaces in a line don't show up as more than one, but are saved as more than one
- Task description should be visible by default in task detail
    > Visibility should be on the description by default?
- No obvious way to know if there are more items in a list off-screen
    > Lowest item should be "..." if more items
- Date should update if taskell is left open
- Blank trello token should show info about setting it up rather than auth error
    > Auth error should show setup info too?
- A single issue with config.ini reverts to defaultConfig?

## Features

- Some way to just see tasks with due dates
    * [ ] Sort by date or filter by urgency?
- Add a List widget for common actions between tasks and sub-tasks
    * [ ] Re-ordering subtasks
- Duplicate task with `+`
- Add tags/labels with `t`
- Use proper error codes
- Performance with large files
    > Becomes unusable with large files
    * [x] Initially use debouncing to avoid writing too often
    * [ ] Cache formatting results
    * [ ] Invalidate layout cache less frequently
    * [ ] Benchmarking tests
    * [ ] Allow cancelling write to avoid trying to write the same file at the same time
- Should be able to have new-lines in task descriptions
    * [x] Trello import
    * [ ] Regular input (Shift + Enter for new line?)
    * [x] Markdown parsing
    * [ ] Text line breaks go a bit funny with multi-line descriptions
- Check times work no matter what timezone
- URL field - plus config to run specific command when selected (e.g. `open -a Chrome.app #{url}`)
- Redo functionality
- Always show list title
    > Floating list titles - so you can always see what list you're in
- Make token UX better
    * [ ] Open link automatically?
    * [ ] Ask for token and save to ini file automatically
- Import Issues from GitHub using  labels
- Readline support?
    > Using Haskline: https://rootmos.github.io/main/2017/08/31/combining-brick-and-haskeline.html
- Editable title?
    > Use a `# Title` at top of file and display title somewhere in taskell
- Keep undo between sessions?
- Ability to load a taskell file with custom config.ini settings
    > Either command line arguments for settings or just a `-c other.ini` command
- Inifinite task depth?
    > No reason, other than UX, that sub-tasks can't have sub-tasks.
- Add Trello syncing

## In Progress

- Search navigation issues
    > Issues with navigation when in NORMAL + SEARCH mode
    * [x] Navigating up and down
    * [x] Navigating between lists
    * [ ] Moving task up and down
    * [x] Often nothing is selected when first entering search mode
- Add custom key support
    * [x] Create bindings.ini
    * [x] Update events to use Map from bindings.ini
    * [ ] Check for key conflicts: include keys not explicitly mapped (e.g. 1-9, Esc, Enter)
    * [x] Check for bits of functionality missing a mapping
    * [x] Update Help dialogue with key mappings
    * [x] Needs to support merging with default options so that it's easy to add new default keys in the future
    * [ ] Add keys to Help which aren't in bindings
    * [ ] More detailed error messages for missing/invalid mappings

## Done

- `a` to add
- `e` to edit
- `Space` to mark complete
- `j`/`k`/`up`/`down` to move up/down list
- `h`/`l`/`Left`/`Right` to move between lists
- `q` to quit
- Create taskell.json if doesn't exist
- Move tasks up/down
- Delete with `D`
- No padding on lists
- Order of lists is wrong
- Can't switch lists
- Add doesn't go to bottom of list
- foldr1 in UI/Main will break if no lists
- Cursor support
- Move tasks between lists with `H`/`K`
- Move to next list with `Space`
- Create new list with `N`
- Delete lists with `X`
- Enter while in add mode creates new/Esc leaves add mode
- Rename Tasks to List
- Rename AllTasks to Lists
- Horizontal scrolling
- Scrolling long lists
- If no lists crashes on up/down - using index in AllTasks
- State should return Maybe?
- Select lists with `1-9`
- Run with any correctly formatted json file
- Wrap lines
- UI modules need refactoring
- Horizontal scrolling stops working if a word longer than the column width is entered
- Cursor no longer in the right place
- Space in edit/add mode doesn't move cursor along - bit disconcerting
- If no items in current list, returns a Nothing, so everything dissappears
- CreateList mode doesn't display anything
- Cursor doesn't show on CreateList mode
- Reordering lists with `>` and `<`
- `o` to add on line below
- `O` to add on line above
- `G` take to bottom of file
- Undo with `u`
- Should only save after adding a new item or finished editing an item - should save when pressing Enter while adding new items
- `C` to change task - i.e. deletes text and goes into edit mode
- Space moves to next and stays in list / H & L move but keep current
- Cursor position assumes single line list title
- `E` edits list title
- Pressing enter on Edit creates new task
- Reordering leftmost list left takes you to last list
- Linux binary
- Homebrew support
- Tabs in tasks throw off cursor and wrapping
- Cursor vertical offset is wrong when adding a new list
- Search using `/`
- Change foldl to foldl`
- Debian package
- Empty tasks aren't obviously selectable
- Pressing `e` on a blank list breaks things
- Add support for Markdown
- Fixed Unicode support
- Scrolling
- New list outside view doesn't scroll
- New item outside view doesn't scroll
- Vertical scrolling falls behind
- Use concurrency for IO
- Search UI
- Vertical scrolling hides list titles
- Use Brick for UI
- `C` doesn't work properly
- Custom colours
- `.taskell` config file in home directory
- Rename Persistence to IO.Taskell
- List titles sometimes go missing
- Use Template Haskell to import in config file templates
- On `?` show keyboard commands
- Remove size from state
- Sub-lists
    * [x] Scrolling in sub-tasks
    * [x] Press Enter to create next
    * [x] Word wrapping
    * [x] Searching
    * [x] Delete items
- No cursor in sub-task view
    * [x] Single line
    * [x] Multi-line
- Customisable Markdown format
    * [x] Change top level headers
    * [x] Change top level list item: e.g. to H3 instead of li
    * [x] Change sub-list: e.g. from "    *" to "-"
- Feels sluggish in sub-task view - cache main view?
- Leaving search only refreshes current list
- Display a warning if any line of the file could not be parsed - otherwise could lead to data loss
- One bad config line stops all config from working - needs to merge with defaultConfig
- Split up Draw/Modal code into more logical chunks
- Move between lists with `m` - shows possible lists
- Caching issue when using `m` to move lists - doesn't update previous list
- Copy and paste?
- Pressing Enter on empty list shows an subtasks box with an error
- Cursor goes missing on the left hand side at the end of a line - needs to wrap
- Sub-task count not visible on last item in a list longer than the vertical height
- Vertical spacing doesn't work if the current item is blank
- Empty tasks - i.e. just a space - don't show up
- Editing list title doesn't always have visibility
- Left/Right arrow keys in insert mode
- Share code between tasks and sub-tasks lists?
    * [x] Move wrap into Field widget
    * [x] Use Field for search
    * [x] Use Field for sub-tasks
    * [x] Use Field for titles
    * [x] Use Field for tasks
    * [x] Make sure `C` works
- Copy and paste
    * [x] List titles
    * [x] Search
- Multiple spaces at the beginning of a line can break cursor positioning
- Task body - e.g. as well as sub lists, have a longer description
- Indicator for when a task has a description
    > Use ≡?
- Pressing `Esc` when entering task description shouldn't reset it
- Better Trello import errors - e.g. auth vs. parsing issues
    * [x] Error on parse issues
    * [x] Error on Auth issues
- Add due dates to tasks with `@`
    * [x] Render due dates
    * [x] Editable due dates
- Trello dates need to take current timezone into account
    > Trello gives dates in UTC, but need to display them in the current timezone. Deadlines should also take timezones into account if necessary.
- Move to column only works for columns before the one you're in
- Add Trello import
    * [x] Basic trello import
    * [x] Add due date support
    * [x] Add sub-tasks support
    * [x] Add card summary support
- Improve Trello checklist import
    * [x] Take checklist fetch errors into account
    * [x] Refactor code
    * [x] Use Reader to pass around trello token?
- Should change list numbering to letters when in move list mode
- GitHub checklist support - []/[x]
- Caching doesn't clear properly when using `o` and `O`
- Add description status indicator option to config.ini
    > Part of the themeing should allow changing to different icon - might not work in all fonts
- Add Twitter links
    * [x] Website
    * [x] Github
- Move image to taskell.app
    > The demo image should live on the taskell.app server, rather than being on an orphan branch on GitHub
    * [x] Move image
    * [x] Update site
    * [x] Update GitHub README.md
    * [x] Remove img branch
- Sort out Homebrew forumula
    > Make the necessary changes so that taskell can be put on the homebrew-core repository
    * [x] Find someone to submit it
    * [x] Use `install_cabal_package`
    * [x] Use `depends_on "cabal-install" => :build`
- Update Task field naming
    * [x] Task: description -> name/title
    * [x] Task: summary -> description
    * [x] UI.Modal.SubTasks -> UI.Modal.Detail
- The isBlank check on tasks could potentially delete a task with no description but which does have sub-tasks
- Blank task names should appear as something
- Use lenses for nested data
- Add more tests
    * [x] Trello response parsing
- Add GitHub Project support
- Refactor IO.Taskell
    > Avoid repeating basically the same code for Trello and GitHub fetching
- Add ability to list GitHub projects
    > Give an organisation or username and repo, list the possible projects to fetch - avoid having to look up the project ID manually first
- GitHub import should take pagination into account
- Use XDG spec for storing config files
- Automate website publishing when doing a new build
    > Should automatically update the `_config.yml` file, build the website, then deploy it
- Can't remove a description
- Title bar for extra info
    * [x] File path
    * [x] Current position
- Search should be case insensitive
- Add Mode to status bar
- Modals interfere with status bar
- Showing a specific task in search mode shows wrong task
    > Based on the index in the full list, rather than the filtered one. So will show the task from the full list if the indexes don't happen to match.
- Can't remove dates
- Tidy up load functions in IO.Taskell
