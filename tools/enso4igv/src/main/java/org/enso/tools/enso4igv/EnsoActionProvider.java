package org.enso.tools.enso4igv;

import java.io.File;
import java.util.Arrays;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import org.netbeans.api.extexecution.ExecutionDescriptor;
import org.netbeans.api.extexecution.ExecutionService;
import org.netbeans.spi.project.ActionProvider;
import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.lookup.ServiceProvider;

import org.netbeans.api.extexecution.base.ProcessBuilder;
import org.netbeans.spi.project.ActionProgress;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;
import org.openide.util.NbPreferences;
import org.openide.windows.IOProvider;

@NbBundle.Messages({
    "CTL_EnsoWhere=Enso Executable Location",
    "CTL_EnsoExecutable=enso/enso.bat",
    "# {0} - executable file",
    "MSG_CannotExecute=Cannot execute {0}",
    "# {0} - executable file",
    "# {1} - exit code",
    "MSG_ExecutionError=Process {0} finished with exit code {1}"
})
@ServiceProvider(service = ActionProvider.class)
public final class EnsoActionProvider implements ActionProvider {

    @Override
    public String[] getSupportedActions() {
        return new String[]{ActionProvider.COMMAND_RUN_SINGLE, ActionProvider.COMMAND_DEBUG_SINGLE};
    }

    @Override
    public void invokeAction(String action, Lookup lkp) throws IllegalArgumentException {
        var process = ActionProgress.start(lkp);
        var script = FileUtil.toFile(lkp.lookup(FileObject.class));


        var io = IOProvider.getDefault().getIO(script.getName(), false);
        var  dd = DialogDisplayer.getDefault();

        var prefs = NbPreferences.forModule(EnsoActionProvider.class);
        var exeKey = "enso.executable";

        var exe = prefs.get(exeKey, "");
        var nd = new NotifyDescriptor.InputLine(Bundle.CTL_EnsoExecutable(), Bundle.CTL_EnsoWhere());
        nd.setInputText(exe);

        var builderFuture = dd.notifyFuture(nd).thenApply(exec -> {
            var file = new File(exec.getInputText());
            if (file.canExecute()) {
                prefs.put(exeKey, file.getPath());

                var b = ProcessBuilder.getLocal();
                b.setExecutable(file.getPath());
                b.setArguments(Arrays.asList("--run", script.getPath()));
                b.setWorkingDirectory(script.getParent());
                b.setRedirectErrorStream(true);
                return b;
            }
            var msg = Bundle.MSG_CannotExecute(file.getPath());
            throw new IllegalArgumentException(msg);
        });

        var waitForProcessFuture = builderFuture.thenCompose((builder) -> {
            var cf = new CompletableFuture<Integer>();
            var descriptor = new ExecutionDescriptor()
                .frontWindow(true).controllable(true)
                .inputOutput(io)
                .postExecution((exitCode) -> {
                    cf.complete(exitCode);
                });
            var service = ExecutionService.newService(builder, descriptor, script.getName());
            service.run();
            return cf;
        });

        waitForProcessFuture.thenAcceptBoth(builderFuture, (exitCode, builder) -> {
            if (exitCode != 0) {
                dd.notifyLater(new NotifyDescriptor.Message(Bundle.MSG_ExecutionError(builder.getDescription(), exitCode), NotifyDescriptor.ERROR_MESSAGE));
            }
            process.finished(exitCode == 0);
        }).exceptionally((ex) -> {
            process.finished(false);
            if (ex instanceof CompletionException && ex.getCause() instanceof CancellationException) {
                return null;
            }
            dd.notifyLater(new NotifyDescriptor.Message(ex.getMessage(), NotifyDescriptor.ERROR_MESSAGE));
            return null;
        });
    }

    @Override
    public boolean isActionEnabled(String string, Lookup lkp) throws IllegalArgumentException {
        if (lkp.lookup(EnsoDataObject.class) != null) {
            return true;
        } else {
            var fo = lkp.lookup(FileObject.class);
            return fo != null && fo.getLookup().lookup(EnsoDataObject.class) != null;
        }
    }
}
