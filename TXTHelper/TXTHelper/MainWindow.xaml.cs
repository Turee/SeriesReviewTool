using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace TXTHelper
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        TXTHelperViewModel dataContext = null;
        public MainWindow()
        {
            InitializeComponent();
            var dc = new TXTHelper.TXTHelperViewModel();
            this.DataContext = dc;
            dataContext = dc;
            this.KeyUp += MainWindow_KeyUp;
        }

        void MainWindow_KeyUp(object sender, KeyEventArgs e)
        {
            switch (e.Key)
            {
                case Key.F1:
                    if (dataContext.DiscardSeriesCommand.CanExecute(null)) dataContext.DiscardSeriesCommand.Execute(null);
                    break;
                case Key.F2:
                    if (dataContext.AcceptSeriesCommand.CanExecute(null)) dataContext.AcceptSeriesCommand.Execute(null);
                    break;
            }
            

        }
    }
}
