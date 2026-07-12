dotnet tool uninstall -g defacto 
dotnet pack && dotnet tool install -g --add-source bin/Release defacto
