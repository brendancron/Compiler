use std::collections::HashMap;
use std::hash::Hash;

pub type TaskId = usize;

#[derive(Debug)]
pub struct Task<Event> {
    pub remaining: usize,
    pub event: Event,
}

#[derive(Debug)]
pub struct DependencyScheduler<Dependency, Event>
where
    Dependency: Eq + Hash + Clone,
{
    dependency_graph: HashMap<Dependency, Vec<TaskId>>,
    task_graph: HashMap<TaskId, Task<Event>>,
    next_id: TaskId,
}

impl<Dependency, Event> DependencyScheduler<Dependency, Event>
where
    Dependency: Eq + Hash + Clone,
{
    pub fn new() -> Self {
        Self {
            dependency_graph: HashMap::new(),
            task_graph: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn add_task(&mut self, deps: &[Dependency], event: Event) -> TaskId {
        let id = self.next_id;
        self.next_id += 1;

        self.task_graph.insert(
            id,
            Task {
                remaining: deps.len(),
                event,
            },
        );

        for dep in deps {
            self.dependency_graph
                .entry(dep.clone())
                .or_default()
                .push(id);
        }

        id
    }

    pub fn resolve_dependency(&mut self, dep: Dependency) -> Vec<Event> {
        let mut ready = Vec::new();

        if let Some(tasks) = self.dependency_graph.remove(&dep) {
            for task_id in tasks {
                if let Some(task) = self.task_graph.get_mut(&task_id) {
                    task.remaining -= 1;

                    if task.remaining == 0 {
                        let task = self.task_graph.remove(&task_id).unwrap();
                        ready.push(task.event);
                    }
                }
            }
        }

        ready
    }
}
