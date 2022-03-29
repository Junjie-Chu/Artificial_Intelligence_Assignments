import gym
import numpy as np
import pandas as pd

env = gym.make("Taxi-v3").env

env.reset() # reset environment to a new, random state
env.render()

print("Action Space {}".format(env.action_space))
print("State Space {}".format(env.observation_space))

state = env.encode(3, 1, 2, 0) # (taxi row, taxi column, passenger index, destination index)
print("State:", state)

env.s = state
env.render()

class cab:
  
  def __init__(self, thestates, theactions, alpha=0.1, gamma=0.6):
    self.theactions = theactions
    self.q_table = np.zeros((thestates, theactions))
    self.epsilon = 0.1
    self.alpha = alpha # learning rate
    self.gamma = gamma # discount rate

    
  def get_action(self, explore=True):
    exploration_tradeoff = np.random.uniform(0, 1)
    
    if explore and exploration_tradeoff < self.epsilon:
      # exploration
      return np.random.randint(self.theactions)    
    else:
      # exploitation (taking the MAX Q value for this state)
      return np.argmax(self.q_table[state, :])
  
  def learn(self, state, action, reward, next_state):
    # Update Q(s,a):= Q(s,a) + lr [R(s,a) + gamma * max Q(s',a') - Q(s,a)]
    self.q_table[state, action] = self.q_table[state, action] + self.alpha * (reward + self.gamma * np.max(self.q_table[new_state, :]) - self.q_table[state, action])

episodes = 1000000
agent = cab(env.observation_space.n, env.action_space.n) 

rewards = []

for episode in range(episodes):
    state = env.reset()
    episode_rewards = []
    
    while True:
            
        action = agent.get_action()
        
        # Take the action (a) and observe the outcome state(s') and reward (r)
        new_state, reward, done, info = env.step(action)
        
        agent.learn(state, action, reward, new_state)

        state = new_state
        
        episode_rewards.append(reward)
        
        if done == True:
          break
          
    rewards.append(np.mean(episode_rewards))

submit = agent.q_table.ravel()

list1=list(range(1,3001))
list2=list(submit)
test1=pd.DataFrame(columns=['Id'],data=list1)
test2=pd.DataFrame(columns=['Value'],data=list2)
test = pd.concat([test1,test2],axis=1)
test.to_csv('F:/submission.csv',index=False) 

