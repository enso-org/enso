# Component Browser Filtering Benchmarks

You can run performance tests for the component browser filtering
(`filtering.bench.ts`) by running `vitest bench` from the `app/gui` directory:

```bash
npx vitest bench
```

Benchmarks require some mock data as suggestion database in `suggestiondb`
directory.

If multiple JSON files are present there, tests will be run mutliple times, one
for each file.

By default, only empty file `empty.json` is provided. You can
[download](https://drive.google.com/drive/folders/1Cej-2Cx1ABNzQhvdHMgzctiwEuuCxkTY?usp=sharing)
prepared suggestion database snapshots or prepare your own by instructions
below.

## Snapshot suggestion database

Unfortunately we don’t provide easy-to-use API for creating suggestion database
snapshots. You can apply a patch and get the snapshot from the dev console:

```patch
diff --git a/app/gui/src/project-view/components/GraphEditor.vue b/app/gui/src/project-view/components/GraphEditor.vue
index 7e15eaffb5..60dec950a1 100644
--- a/app/gui/src/project-view/components/GraphEditor.vue
+++ b/app/gui/src/project-view/components/GraphEditor.vue
@@ -90,6 +90,7 @@
   widgetRegistry.loadWidgets(Object.entries(builtinWidgets))
   if (import.meta.env.DEV) {
     ;(window as any).suggestionDb = toRaw(suggestionDb.entries)
+    ;(window as any).synchronizer = suggestionDb._synchronizer
   }
 })

diff --git a/app/gui/src/project-view/stores/suggestionDatabase/index.ts b/app/gui/src/project-view/stores/suggestionDatabase/index.ts
index ee74e3a37b..a4861962ac 100644
--- a/app/gui/src/project-view/stores/suggestionDatabase/index.ts
+++ b/app/gui/src/project-view/stores/suggestionDatabase/index.ts
@@ -155,6 +155,7 @@

 class Synchronizer {
   queue: AsyncQueue<{ currentVersion: number }>
+  private updateProcessorReady: SuggestionUpdateProcessor | undefined

   constructor(
     projectStore: ProjectStore,
@@ -175,6 +176,10 @@
     this.queue = new AsyncQueue(initState)
   }

+  exportEntries() {
+    return this.updateProcessorReady?.exportEntries()
+  }
+
   static async loadDatabase(
     entries: SuggestionDb,
     lsRpc: LanguageServer,
@@ -240,6 +245,7 @@
     // Before an new update is received, apply all queued updates from before initialization.
     earlyUpdates.forEach(processUpdate)
     earlyUpdates.length = 0
+    this.updateProcessorReady = updateProcessor
   }
 }

diff --git a/app/gui/src/project-view/stores/suggestionDatabase/lsUpdate.ts b/app/gui/src/project-view/stores/suggestionDatabase/lsUpdate.ts
index 2e771e6a8e..1dc54605e2 100644
--- a/app/gui/src/project-view/stores/suggestionDatabase/lsUpdate.ts
+++ b/app/gui/src/project-view/stores/suggestionDatabase/lsUpdate.ts
@@ -540,8 +540,17 @@
   constructor(
     private readonly groups: ToValue<DeepReadonly<GroupInfo[]>>,
     private readonly projectNames: ProjectNameStore,
+    private entriesToExport: lsTypes.SuggestionEntry[] = [],
   ) {}

+  exportEntries() {
+    return this.entriesToExport
+  }
+
+  updateEntriesToExport(lsEntry: lsTypes.SuggestionEntry) {
+    this.entriesToExport.push(lsEntry)
+  }
+
   /** Create a suggestion DB entry from data provided by the given language server. */
   entryFromLs(lsEntry: lsTypes.SuggestionEntry): Result<SuggestionEntry> {
     return withContext(
@@ -580,6 +589,7 @@
         return withContext(
           () => `when adding new entry ${JSON.stringify(update)}`,
           () => {
+            this.updateEntriesToExport(update.suggestion)
             const newEntry = this.entryFromLs(update.suggestion)
             if (!newEntry.ok) return newEntry
             entries.set(update.id, newEntry.value)
```

After applying the patch, load some project and run
`synchronizer.exportEntries()` in the dev console.
