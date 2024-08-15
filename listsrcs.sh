lr *.txt *.org *.lsp *.scm *.rst *.com *.sh descrip.mms | tee ~usr/sstg.lis | sedit 's/%{?+}$/ar uv sstg.w $1/' | tee ~usr/initsstg.sh
