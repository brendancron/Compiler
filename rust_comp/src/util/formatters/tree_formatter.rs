pub trait TreeNode {
    type Child;

    fn label(&self) -> String;
    fn children(&self) -> Vec<Self::Child>;
}
