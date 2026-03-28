import os
import shutil

dep = "deployment"

def createdir(dirname):
    if not os.path.exists(dirname):
        os.mkdir(dirname)

createdir(dep)

cwd = os.getcwd()

def copyfrom(dirname):
    EXCLUSION = ['deployment', 'testers', 'outputs', 'prompts']
    EXPORTS = ['.js', '.html', '.h']
    directory = os.path.join(cwd, dirname)
    print("Handling directory:",directory)
    for file in os.listdir(directory):
        path = os.path.join(directory, file)
        ziel = os.path.join(cwd, dep, dirname, file)
        #print(cwd,dep,dirname,file)
        #os.system("pause")
        if os.path.isfile(path):
            
            if not any([path.endswith(i) for i in EXPORTS]):
                continue
            print("Handling file:",path,"into",ziel)
            if path.endswith(".js") or path.endswith(".html"):
                fn = open(path, "r", encoding='utf-8')
                target = open(ziel, "w", encoding='utf-8')
                while True:
                    data = fn.readline()
                    if data == '':
                        break
                    data = data.replace("\"./", "\"/mindustry_c_compiler/").replace("/include/", "/mindustry_c_compiler/include/")
                    target.write(data)
                fn.close()
                target.close()
            else:
                shutil.copy(path, ziel)
        elif os.path.isdir(path):
            if file not in EXCLUSION and (not (file == '' or file[0] == '.')):
                createdir(ziel)
                copyfrom(file)

copyfrom('')