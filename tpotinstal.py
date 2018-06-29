print ('Hello, world!')
import pip._internal as pip

package_names=['deap','update_checker', 'tqdm', 'stopit'] #packages to install
pip.main(['install'] + [package_names] + ['--upgrade']) 
package_names=['xgboost'] #packages to install
pip.main(['install'] + [package_names] + ['--upgrade']) 
package_names=['scikit-mdr','skrebate'] #packages to install
pip.main(['install'] + [package_names] + ['--upgrade']) 
package_names=['tpot'] #packages to install
pip.main(['install'] + [package_names] + ['--upgrade']) 



