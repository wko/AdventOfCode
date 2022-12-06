module FordFulkerson where


import Control.Lens
-- Import the Data.List module to use the maximumBy and minimumBy functions
import Data.List ( maximumBy )
import Data.Ord ( comparing )
-- Define the type of a flow network as a list of edges, where each edge
-- is a tuple containing the source, destination, and capacity of the edge
type FlowNetwork = [(Int, Int, Int)]

-- Define the function that finds the maximum flow in a flow network
maxFlow :: FlowNetwork -> Int -> Int -> Int
maxFlow network source sink = maxFlow' network source sink [] 0
  where
    -- Define a recursive function that finds the maximum flow in a flow network
    maxFlow' :: FlowNetwork -> Int -> Int -> FlowNetwork -> Int -> Int
    maxFlow' network source sink path flow
      -- If the sink is reached, add the flow through the path to the current flow
      -- and return the updated flow
      | sink `elem` map (\(_, b, _) -> b) path = maxFlow' network source sink [] (flow + flowThroughPath path)

      -- If no more paths can be found, return the current flow
      | null nextPaths = flow

      -- Otherwise, find the path with the maximum flow and continue the search
      | otherwise = maxFlow' network source sink (maximumBy (comparing flowThroughPath) nextPaths) flow
      where
        -- Find all the paths from the source that can be extended
        paths = filter (\(a, _, _) -> a `elem` map (\(x, y, z) -> y) path) network

        -- Find all the paths that can be extended from the source, by taking the path with the
        -- minimum capacity and extending it with each edge that connects to the destination of the path
        nextPaths = concatMap (\(a, b, c) -> map (\(x, y, z) -> [(a, b, c), (x, y, z)]) $ filter (\(x, _, _) -> x == b) network) paths

        -- Calculate the flow through a path as the minimum capacity of all the edges in the path
        flowThroughPath :: FlowNetwork -> Int
        flowThroughPath path = minimum $ map (\(_, _, c) -> c) path

-- Test the maxFlow function
test :: IO ()
test = do
  let network = [(1, 2, 10), (1, 3, 5), (2, 3, 3), (2, 4, 7), (3, 5, 2), (4, 5, 6)]
  print (maxFlow network 1 2)
  print (maxFlow network 1 5)  -- should print 10