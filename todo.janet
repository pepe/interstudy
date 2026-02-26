#!/usr/bin/env janet
(def tasks-file "tasks.janet")

(defn load-tasks []
  (if (os/stat tasks-file)
    (-> tasks-file slurp parse eval)
    @[]))

(defn save-tasks [tasks]
  (spit tasks-file (string/format "%q" tasks)))

(defn add-task [text]
  (def tasks (load-tasks))
  (array/push tasks @{:text text :done false})
  (save-tasks tasks)
  (print "Added task: " text))

(defn list-tasks []
  (def tasks (load-tasks))
  (loop [i :range [0 (length tasks)]]
    (def task (tasks i))
    (print (string (+ i 1) ". "
                   (if (task :done) "[x] " "[ ] ")
                   (task :text)))))

(defn done-task [index]
  (def tasks (load-tasks))
  (if (and (> index 0) (<= index (length tasks)))
    (do
      (put (tasks (- index 1)) :done true)
      (save-tasks tasks)
      (print "Completed task " index))
    (print "Invalid task number")))

(def args (array/slice (dyn :args) 1))
(match args
  ["add" text] (add-task text)
  ["list"]     (list-tasks)
  ["done" n]   (done-task (scan-number n))
  _ (print "Usage:\n  add \"task\"\n  list\n  done <number>"))