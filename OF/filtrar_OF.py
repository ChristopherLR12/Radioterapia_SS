import tkinter as tk
from tkinter import filedialog
import os

# Crear una ventana principal oculta
root = tk.Tk()
root.withdraw()  # Ocultar la ventana principal


file_path = filedialog.askopenfilename(
    title="Selecciona un archivo",
    filetypes=[("Archivos txt", "*.txt"),("Archivos csv", "*.csv"),]
)

if file_path:
    save_path = filedialog.asksaveasfilename(
        title="Guardar archivo como",
        defaultextension=".csv",
        filetypes=[("Archivo CSV", "*.csv")],
        initialfile=f'filtrado_{os.path.splitext(os.path.basename(file_path))[0]}.csv'
    )

    if save_path:
        with open(file_path, 'r') as raw:
            with open(save_path, 'w') as data:
                for line in raw.readlines():
                    if line[0].isdigit():
                        data.write(line)
        
        print(f"Archivo filtrado guardado como: {save_path}")
    else:
        print("No se seleccionó una ubicación para guardar el archivo.")
else:
    print("No se seleccionó ningún archivo.")
