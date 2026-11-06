# Getting started with Triangulator on a new computer

This guide takes you from a brand-new computer to running Triangulator. You don't need any coding
experience: you'll copy and paste three lines. Allow about **15 minutes** the first time. After that,
starting the app takes a few seconds.

You'll install two free programs:

- **R**: the engine Triangulator runs on. You won't use it directly.
- **RStudio**: a friendly window for R. This is where you'll paste the three lines.

> **Work laptop?** Some company computers don't let you install programs. If an installer asks for an
> administrator password you don't have, ask your IT team to install **R** and **RStudio Desktop**
> (both free) for you, then carry on from Step 3.

---

## Step 1: Install R

1. Go to **https://cloud.r-project.org**
2. Click the link for your computer:
   - **Windows:** click *Download R for Windows*, then *base*, then the big *Download R-x.x.x for Windows* link at the top.
   - **Mac:** click *Download R for macOS*. Pick the right file for your Mac:
     - Newer Macs (M1, M2, M3, M4 chip): the file ending **arm64.pkg**
     - Older Macs (Intel chip): the file ending **x86_64.pkg**
     - *Not sure?* Click the Apple menu, then **About This Mac**, and look at **Chip** or **Processor**.
3. Open the downloaded file and click **Next / Continue** through the installer, accepting all the
   standard options.

## Step 2: Install RStudio

1. Go to **https://posit.co/download/rstudio-desktop/**
2. Click the **Download RStudio Desktop** button for your computer (the website usually picks the right one).
3. Open the downloaded file and install it:
   - **Windows:** click through the installer, accepting the standard options.
   - **Mac:** drag the RStudio icon into your **Applications** folder.

## Step 3: Open RStudio and find the Console

Open **RStudio** (not "R"). You'll see a window with several panels. The one on the left (or bottom
left) is the **Console**: its tab is labelled *Console*, and it has a `>` symbol at the bottom where
you can type.

To run a line: **copy it from this page, click in the Console, paste it, and press Enter.**

## Step 4: Install Triangulator (one time only)

Paste this line into the Console and press **Enter**:

```r
install.packages("remotes")
```

Wait until the `>` symbol comes back (around a minute). Then paste this line and press **Enter**:

```r
remotes::install_github("MarkSheridanFSAI/Triangulator_V2", upgrade = "never")
```

This downloads Triangulator and the free add-ons it uses. **The first time can take 5 to 10 minutes.**
Lots of text will scroll past; that's normal. It's finished when the `>` symbol comes back and you see
a line like `* DONE (Triangulator)`.

If you're asked a question in the Console while it installs:

- *"Do you want to install from sources the packages which need compilation?"*: type `no` and press Enter.
- *"Would you like to use a personal library instead?"*: type `yes` and press Enter.

## Step 5: Start the app

Paste this line and press **Enter**:

```r
Triangulator::run_app()
```

Triangulator opens in your web browser. Click **Try it with sample data** to explore with example
claims, or **Browse...** to upload your own Excel or CSV file.

**To stop the app:** close the browser tab, go back to RStudio and press **Esc** (or click the red
**Stop** sign above the Console).

## Next time

Open RStudio, paste `Triangulator::run_app()` into the Console and press Enter. That's it.

**To get the latest version** at any time, run the Step 4 `remotes::install_github(...)` line again.

---

## Using your own data

- One row per claim. You need columns for: claim ID, valuation date (the bordereau or listing date),
  incident date, report date, paid to date and outstanding reserve. Policy ID, policy start date and
  grouping columns (such as class of business) are optional.
- Triangulator guesses which column is which from the headings. Check its guesses in step 2 of the
  Import page.
- **Bordereaux:** one bordereau is enough for the summary, claim sizes and group comparisons. For
  triangles and reserve estimates, paste several bordereaux one under another in a single file, with a
  column holding each bordereau's date, and choose that column as the **Valuation date**.
- Your data stays on your computer. Nothing is uploaded to the internet.

## If something goes wrong

| What you see | What to do |
|---|---|
| `there is no package called 'Triangulator'` | Step 4 didn't finish. Run the `remotes::install_github(...)` line again and wait for `DONE`. |
| `there is no package called 'remotes'` | Run `install.packages("remotes")` first. |
| `HTTP error 404` or `Failed to install` | Check the line was copied exactly, including the quote marks. If it still fails, your network may block GitHub; try another network or ask IT. |
| A message about **Rtools** (Windows) | You can ignore it. Triangulator doesn't need Rtools. |
| `API rate limit exceeded` | GitHub limits how often you can download. Wait an hour and try again. |
| The browser doesn't open | Look in the Console for a line like `Listening on http://127.0.0.1:1234` and paste that address into your browser. |
| The app shows an error message | Read the message: it usually says what to fix in your file. If you're stuck, report it on the [Issues page](https://github.com/MarkSheridanFSAI/Triangulator_V2/issues) with a copy of the red text from the Console (but never share confidential data). |

Happy analysing!
